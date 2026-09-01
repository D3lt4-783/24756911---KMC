/*
  =======================================
  Neonatal Mattress Breathing Simulation 
  =======================================

  HOW THE UI VALUES MAP ONTO THE BREATHING/PUMP MODEL
  ----------------------------------------------------
    bpm             -> pump heart-rate. Sets PERIOD_MS / ON_TIME_MS.
                        Unrelated to breathing - only controls the pump/piezo
                        experiment.
    respirationPM      -> breaths per minute. Sets T_CYCLE = 60 / respirationPM.
                        The 2:3 inhale:exhale time split is preserved.
    amplitude        -> the scaling factor the raw Singh et al. displacement
                        is multiplied by before conversion to steps.
    baseHeight       -> constant baseline offset (mm) the breathing
                        oscillation rides on top of. Reached via the gentle
                        RISING phase before breathing motion begins, and
                        must stay in the running-loop formula too (this
                        was the v3 -> v4 bug).
    durationMinutes  -> total session length. When it elapses, the system
                        automatically runs the same homing sequence as
                        pressing STOP.

  ===============================
  WIRING — ARDUINO MEGA PIN MAP
  ================================
    Stepper 1:  ENA+ = 32   DIR+ = 30   PUL+ = 28
    Stepper 2:  ENA+ = 26   DIR+ = 24   PUL+ = 22
    Pump:       DIR  = 2    PWM  = 3    SYNC = 4    RUN (to Uno) = 5

    LCD (I2C, 16x2, PCF8574 backpack):
        SDA -> Mega pin 20   *** hardware I2C bus on the Mega
        SCL -> Mega pin 21       like on the Uno.
        VCC -> 5V, GND -> GND

    Potentiometer (10k):
        Wiper -> A0, outer legs -> 5V and GND

    SELECT button: one leg -> pin 6, other leg -> GND (internal pull-up)
       *** MOVED BACK from pin 5 — pin 5 is RUN_PIN and this was causing
           the restart-after-stop bug (see v4 changelog above).

    START button:  one leg -> pin 7, other leg -> GND (internal pull-up)
       *** MOVED BACK from pin 4 — pin 4 is SYNC_PIN, same issue.

    STOP button:   one leg -> pin 11, other leg -> GND (internal pull-up).
       Only has an effect while the system is SYS_RUNNING.

    LIMIT_SW1 (motor 1 side): one leg -> pin 13, other leg -> GND
    LIMIT_SW2 (motor 2 side): one leg -> pin 12, other leg -> GND
    
       Both internal pull-up, reads LOW when triggered. 

  Final pin map: 2,3,4,5 (pump), 6,7,11 (select/start/stop)
  , 12,13 (limit switches), 20,21 (I2C), 22,24,26,28,30,32
  (steppers), A0 (pot).
*/

#include <math.h>
#include <Wire.h>
#include <LiquidCrystal_I2C.h>

// LCD
LiquidCrystal_I2C lcd(0x27, 16, 2);

// PIN DEFINITIONS
// RSS and PSS hardware
const uint8_t ENA1 = 32, DIR1 = 30, PUL1 = 28;   // Stepper 1
const uint8_t ENA2 = 26, DIR2 = 24, PUL2 = 22;   // Stepper 2
const uint8_t DIR_PIN = 2, PWM_PIN = 3;          //Pump driver
const uint8_t SYNC_PIN = 4;                       //Per-beat sync pulse to Uno       (Can remove, only for testing)
const uint8_t RUN_PIN  = 5;                       // Tells Uno to start/stop recording(Can remove, only for testing)

// User interface hardware
//LCD SDA/SCL are the Mega's hardware I2C pins 20/21, wired directly.
const int POT_PIN = A0;
const int SELECT_BUTTON = 6;   
const int START_BUTTON  = 7;
const uint8_t STOP_BUTTON = 11;  

//Limit switches
const uint8_t LIMIT_SW1 = 13;   // home switch, motor 1 side
const uint8_t LIMIT_SW2 = 12;   // home switch, motor 2 side

// SYSTEM STATES
// SYS_CONFIG   - user is dialing in settings with the pot + SELECT button
// SYS_STARTING - "confirm settings" screen sequence
// SYS_RISING   - gentle ramp up to baseHeight before breathing starts
// SYS_RUNNING  - breathing + pump control loops are active
// SYS_STOPPING - homing to the limit switches (STOP button or duration end)
// SYS_DONE     - homed and disabled; press START to configure a new session
enum SystemState { SYS_CONFIG, SYS_STARTING, SYS_RISING, SYS_RUNNING, SYS_STOPPING, SYS_DONE };
SystemState systemState = SYS_CONFIG;

// Sub-state / timer for "settings summary" screens
uint8_t startingScreen = 0;              // 0 = settings summary, 1 = duration
unsigned long startingScreenBeganAt = 0;
const unsigned long STARTING_SCREEN_MS = 3000;

// MENU (SYS_CONFIG only)
int menu = 0;
// 0 = BPM (pump heart rate)
// 1 = RespirationPM (breaths/min)
// 2 = Amplitude (breathing displacement scale factor, mm)
// 3 = Duration (session length)
// 4 = Base Height (baseline mattress offset, mm)
// 5 = Ready

// USER SETTINGS
int bpm= 80;
int respirationPM = 15;
int amplitude = 10;
int durationMinutes = 60;
int baseHeight= 25;

// BUTTON DEBOUNCE

bool lastSelectState = HIGH;
bool lastStartState = HIGH;
bool lastStopState = HIGH;
unsigned long lastButtonPress = 0;
const unsigned long debounceTime = 200;

//rack-and-pinion geometry
const float MOTOR_STEPS_PER_REV = 200.0;
const float MICROSTEPPING = 16.0;
const float GEAR_PITCH_DIAMETER_MM = 34;
const float RACK_TRAVEL_PER_REV_MM = PI * GEAR_PITCH_DIAMETER_MM;

float STEPS_PER_MM;

const unsigned long BREATH_UPDATE_INTERVAL_MS = 5;  // 200 Hz breathing control loop
const uint8_t MAX_STEPS_PER_UPDATE = 5;//breathing-loop safety cap
const unsigned int PULSE_WIDTH_US = 5;

//Gentle-motion  for rise to base height and drop to home homing
const unsigned long RISE_UPDATE_INTERVAL_MS =15;
const uint8_t RISE_STEPS_PER_UPDATE = 1;

const unsigned long HOME_UPDATE_INTERVAL_MS = 15;
const uint8_t HOME_STEPS_PER_UPDATE = 1;

// PSS timing (derived from user-set `bpm` at session start)
const int TOTAL_BEATS = 100; //for testing purposes only, can replace/remove for standard opperation 
const int PWM_VALUE = 192;   
const int SYNC_PULSE_MS = 20;   // for testing to sync with Arduino UNO, can remove. 
int PERIOD_MS;
int ON_TIME_MS;

// RSS model params. Singh et. al model 

float T1, T2, T_CYCLE;
const float RESP_A2= -5.0;
const float RESP_A1 = 10.0;
const float RESP_A0 = 0.0;
const float TAU_RS = 0.5;
const float RRS  = 2.0;
const float TAU_EX = 7.5;
const float V0_INIT = 0.0;

float C1, C2, C3; // do not depend on T1 - computed once in setup()
float P_T1;       // depends on T1 - recomputed in computeDerivedParameters()
float V_T1;        // depends on T1 - recomputed in computeDerivedParameters()
float EXH_DENOM;  // does not depend on T1 - computed once in setup()

// Stepper motor driver State
struct StepperCtrl {
  uint8_t enaPin, dirPin, pulPin;
  long currentSteps;
  long targetSteps;
};

StepperCtrl motor1 = {ENA1, DIR1, PUL1, 0, 0};
StepperCtrl motor2 = {ENA2, DIR2, PUL2, 0, 0};

unsigned long breathStartMillis = 0;
unsigned long lastBreathUpdate  = 0;
unsigned long sessionEndMillis  = 0;  // 0 = no duration limit

long riseTargetSteps = 0;
unsigned long lastRiseUpdate = 0;

bool motor1Homed = false;
bool motor2Homed = false;
unsigned long lastHomeUpdate = 0;

//Pump driver state
enum PumpState { PUMP_IDLE, PUMP_SYNC_HIGH, PUMP_RUNNING_ON };
PumpState pumpState = PUMP_IDLE;
int beatCount = 0;
bool experimentRunning = false;
unsigned long lastBeatTime = 0;
unsigned long beatStartTime = 0;

unsigned long lastRunningDisplay = 0;
const unsigned long RUNNING_DISPLAY_INTERVAL_MS = 1000;

//  RespirationPM displacement model
float breathDisplacement(float t) {
  if (t <= T1) {
    return (TAU_RS / RRS) * (C1 * t * t + C2 * t + C3 * (1.0 - exp(-t / TAU_RS)))
           + V0_INIT * exp(-t / TAU_RS);
  } else {
    float te = t - T1;
    return (P_T1 / EXH_DENOM) * (exp(-te / TAU_EX) - exp(-te / TAU_RS))
           + V_T1 * exp(-te / TAU_RS);
  }
}

//Single-step pulse

void doStep(StepperCtrl &m, bool forward) {
  digitalWrite(m.dirPin, forward ? HIGH : LOW);
  digitalWrite(m.pulPin, HIGH);
  delayMicroseconds(PULSE_WIDTH_US);
  digitalWrite(m.pulPin, LOW);
  delayMicroseconds(PULSE_WIDTH_US);
  m.currentSteps += forward ? 1 : -1;
}

void updateStepper(StepperCtrl &m) {
  uint8_t moved = 0;
  while (m.currentSteps != m.targetSteps && moved < MAX_STEPS_PER_UPDATE) {
    bool forward = (m.targetSteps > m.currentSteps);
    doStep(m, forward);
    moved++;
  }
}

//  Pump
void updatePump() {
  if (!experimentRunning) return;

  unsigned long now = millis();

  switch (pumpState) {

    case PUMP_IDLE:
      if (now - lastBeatTime >= (unsigned long)PERIOD_MS) {
        beatCount++;
        lastBeatTime = now;
        beatStartTime = now;

        digitalWrite(SYNC_PIN, HIGH);
        analogWrite(PWM_PIN, PWM_VALUE);
        pumpState = PUMP_SYNC_HIGH;

        Serial.print("Beat ");
        Serial.println(beatCount);
      }
      break;

    case PUMP_SYNC_HIGH:
      if (now - beatStartTime >= (unsigned long)SYNC_PULSE_MS) {
        digitalWrite(SYNC_PIN, LOW);
        pumpState = PUMP_RUNNING_ON;
      }
      break;

    case PUMP_RUNNING_ON:
      if (now - beatStartTime >= (unsigned long)ON_TIME_MS) {
        analogWrite(PWM_PIN, 0);
        pumpState = PUMP_IDLE;

        if (beatCount >= TOTAL_BEATS) {
          experimentRunning = false;
          digitalWrite(RUN_PIN, LOW);
          Serial.println("Pump calibration burst complete");
        }
      }
      break;
  }
}

//  Derived-parameter computation - called once per session start
void computeDerivedParameters() {
  PERIOD_MS  = 60000 / bpm;
  ON_TIME_MS = max(120,(int)(PERIOD_MS *0.12)); // maximum taken to ensure pulse duration of roughly 100ms
                                                // is produced

  T_CYCLE = 60.0/ respirationPM;
  T1 = T_CYCLE *0.4;
  T2 = T_CYCLE -T1;

  P_T1 = RESP_A2* T1 * T1 + RESP_A1 * T1 + RESP_A0;
  V_T1 = (TAU_RS/ RRS) *(C1 * T1 * T1 + C2 * T1 + C3* (1.0 -exp(-T1 / TAU_RS)))
         +V0_INIT *exp(-T1 / TAU_RS);
}

// SYS_STARTING -> SYS_RISING: Enables the drivers and begins a gentle ramp to baseHeight.
//Breathing/pump timers are not started yet. 

void startRisingPhase() {
  computeDerivedParameters();

  digitalWrite(ENA1, LOW);
  digitalWrite(ENA2, LOW);
  digitalWrite(DIR_PIN, HIGH);
  digitalWrite(SYNC_PIN, LOW);
  analogWrite(PWM_PIN, 0);

  motor1.currentSteps = 0; motor1.targetSteps = 0;
  motor2.currentSteps = 0; motor2.targetSteps = 0;

  riseTargetSteps = lround(baseHeight * STEPS_PER_MM);
  lastRiseUpdate = millis();

  lcd.clear();
  lcd.setCursor(0, 0);
  lcd.print("RISING...");
  lcd.setCursor(0, 1);
  lcd.print("To base height");

  Serial.println("Rising to base height");
  Serial.print("BPM: "); Serial.println(bpm);
  Serial.print("RespirationPM (breaths/min): "); Serial.println(respirationPM);
  Serial.print("Amplitude scale (mm): "); Serial.println(amplitude);
  Serial.print("Base height (mm): "); Serial.println(baseHeight);
  Serial.print("Duration (min): "); Serial.println(durationMinutes);

  systemState = SYS_RISING;
}

//Gentle ramp toward riseTargetSteps. 
void updateRising() {
  bool m1Done =(motor1.currentSteps== riseTargetSteps);
  bool m2Done= (motor2.currentSteps==riseTargetSteps);

  if (m1Done && m2Done) {
    beginRunningPhase();
    return;
  }

  unsigned long now = millis();
  if (now -lastRiseUpdate<RISE_UPDATE_INTERVAL_MS) return;
  lastRiseUpdate = now;

  // both motors target the same riseTargetSteps value. Direction below is computed from riseTargetSteps vs currentSteps
  //directly, so this assignment doesn't drive motion by itself, but it needs to match that comparison rather than being negated.

  motor1.targetSteps = riseTargetSteps; //may need to switch signs depending on wiring i.e. -riseTargetSteps
  motor2.targetSteps = riseTargetSteps; //may need to switch signs depending on wiring i.e. -riseTargetSteps 

  for (uint8_t i = 0; i < RISE_STEPS_PER_UPDATE; i++) {
    if (motor1.currentSteps != riseTargetSteps)
      doStep(motor1, riseTargetSteps > motor1.currentSteps);
    if (motor2.currentSteps != riseTargetSteps)
      doStep(motor2, riseTargetSteps > motor2.currentSteps);
  }
}

//SYS_RISING -> SYS_RUNNING: Base height has been reached; start the breathing clock and pump.
//breathDisplacement(0) == 0, so the very first breathing target equals baseHeight — 
// matching where the rising phase just left off.

void beginRunningPhase() {
  breathStartMillis = millis();
  lastBreathUpdate = 0;
  lastBeatTime =millis();
  beatCount   = 0;
  pumpState = PUMP_IDLE;
  experimentRunning =true;
  digitalWrite(RUN_PIN, HIGH);

  sessionEndMillis = (durationMinutes > 0)
                        ? breathStartMillis + (unsigned long)durationMinutes * 60000UL
                        : 0;

  Serial.println("Breathing + pump session started");
  systemState = SYS_RUNNING;
}

//  Triggers a stop — either from the STOP button while SYS_RUNNING, or
//  automatically when durationMinutes elapses. Stops the pump immediately, then gently homes both motors.

void beginStopSequence() {
  analogWrite(PWM_PIN, 0);
  digitalWrite(RUN_PIN, LOW);
  experimentRunning = false;

  motor1Homed = digitalRead(LIMIT_SW1) == LOW; // already home? then don't move it
  motor2Homed = digitalRead(LIMIT_SW2) == LOW;
  lastHomeUpdate = millis();

  lcd.clear();
  lcd.setCursor(0, 0);
  lcd.print("STOPPING...");
  lcd.setCursor(0, 1);
  lcd.print("Homing motors");

  Serial.println("Stop requested - homing...");

  systemState = SYS_STOPPING;
}

// gentle retract-until-limit-switch homing.
//Each motor stops independently as soon as ITS switch triggers.
void updateHoming() {
  bool sw1 = digitalRead(LIMIT_SW1) == LOW;
  bool sw2 = digitalRead(LIMIT_SW2) == LOW;
  if (sw1) motor1Homed = true;
  if (sw2) motor2Homed = true;

  if (motor1Homed && motor2Homed) {
    digitalWrite(ENA1, HIGH);
    digitalWrite(ENA2, HIGH);

    Serial.println("Homing complete - drivers disabled");

    lcd.clear();
    lcd.setCursor(0, 0);
    lcd.print("STOPPED");
    lcd.setCursor(0, 1);
    lcd.print("Press START");
    systemState = SYS_DONE;
    return;
  }

  unsigned long now = millis();
  if (now - lastHomeUpdate < HOME_UPDATE_INTERVAL_MS) return;
  lastHomeUpdate = now;

  for (uint8_t i = 0; i < HOME_STEPS_PER_UPDATE; i++) {
    // forward=false is taken to be the retract/home direction - verify
    //on the bench and flip if the mattress moves the wrong way.

    if (!motor1Homed) doStep(motor1, false);
    if (!motor2Homed) doStep(motor2, false);
  }
}

// READ POTENTIOMETER (SYS_CONFIG)

void readPotentiometer() {
  int potValue = analogRead(POT_PIN);

  switch (menu) {
    case 0:bpm = map(potValue, 0, 1023,40,120); break;
    case 1:respirationPM =map(potValue,0, 1023, 10, 20);break;
    case 2: amplitude =map(potValue, 0,1023, 8, 15); break;
    case 3: durationMinutes= map(potValue, 0, 1023, 0, 48)* 30; break;
    case 4:baseHeight =map(potValue,0, 1023,0, 50);break;
  }

  displayMenu();
}

// SELECT BUTTON (SYS_CONFIG)
void readSelectButton() {
  bool buttonState = digitalRead(SELECT_BUTTON);

  if (buttonState == LOW &&
      lastSelectState == HIGH &&
      millis() -lastButtonPress >debounceTime) {
    lastButtonPress =millis();

    menu++;
    if (menu>5) menu = 0;

    displayMenu();
  }

  lastSelectState = buttonState;
}

// START BUTTON, single job: SYS_CONFIG (menu==5) -> begin settings confirmation -> RISING -> RUNNING
//   SYS_DONE-> back to SYS_CONFIG for a new session

void readStartButton() {
  bool buttonState = digitalRead(START_BUTTON);

  if (buttonState == LOW &&
      lastStartState == HIGH &&
      millis() - lastButtonPress > debounceTime) {
    lastButtonPress = millis();

    if (systemState == SYS_CONFIG && menu == 5) {
      startingScreen = 0;
      startingScreenBeganAt = millis();
      showSettingsScreen();
      systemState = SYS_STARTING;

    } else if (systemState == SYS_DONE) {
      menu = 0;
      systemState = SYS_CONFIG;
      displayMenu();
    }
  }

  lastStartState = buttonState;
}

// STOP BUTTON — only does anything while SYS_RUNNING.

void readStopButton() {
  bool buttonState = digitalRead(STOP_BUTTON);

  if (buttonState == LOW &&
      lastStopState == HIGH &&
      millis() - lastButtonPress > debounceTime) {
    lastButtonPress = millis();

    if (systemState == SYS_RUNNING) {
      beginStopSequence();
    }
  }

  lastStopState = buttonState;
}

// Display Menu (SYS_CONFIG)

void displayMenu() {
  lcd.clear();

  switch (menu) {
    case 0:
      lcd.setCursor(0,0);lcd.print("> BPM (pump)");
      lcd.setCursor(0,1); lcd.print(bpm);lcd.print(" BPM");
      break;

    case 1:
      lcd.setCursor(0,0);lcd.print("> RespirationPM");
      lcd.setCursor(0,1); lcd.print(respirationPM);lcd.print(" /min");
      break;

    case 2:
      lcd.setCursor(0, 0);lcd.print("> Amplitude");
      lcd.setCursor(0,1); lcd.print(amplitude); lcd.print(" mm");
      break;

    case 3: {
      lcd.setCursor(0, 0);lcd.print("> Duration");
      lcd.setCursor(0,1);

      int hours = durationMinutes / 60;
      int minutes = durationMinutes % 60;
      lcd.print(hours);lcd.print("h ");
      if (minutes < 10) lcd.print("0");
      lcd.print(minutes);lcd.print("m");
      break;
    }

    case 4:
      lcd.setCursor(0,0); lcd.print("> Base Height");
      lcd.setCursor(0,1);lcd.print(baseHeight); lcd.print(" mm");
      break;

    case 5:
      lcd.setCursor(0, 0);lcd.print("SYSTEM READY");
      lcd.setCursor(0,1); lcd.print("Press START");
      break;
  }
}

// Setting confirmation scren (SYS_STARTING)
void showSettingsScreen() {
  lcd.clear();
  if (startingScreen == 0) {
    lcd.setCursor(0, 0);
    lcd.print("B:"); lcd.print(bpm);
    lcd.print(" R:"); lcd.print(respirationPM);
    lcd.setCursor(0,1);
    lcd.print("A:");lcd.print(amplitude);
    lcd.print(" H:");lcd.print(baseHeight);
  } else {
    lcd.setCursor(0, 0);
    lcd.print("Duration:");
    lcd.setCursor(0, 1);
    int hours = durationMinutes /60;
    int minutes = durationMinutes% 60;
    lcd.print(hours); lcd.print("h ");
    if (minutes < 10) lcd.print("0");
    lcd.print(minutes); lcd.print("m");
  }
}

void updateStartingSequence() {
  if (millis() - startingScreenBeganAt >= STARTING_SCREEN_MS) {
    if (startingScreen == 0) {
      startingScreen = 1;
      startingScreenBeganAt = millis();
      showSettingsScreen();
    } else {
      startRisingPhase();
    }
  }
}

// Session display (SYS_RUNNING), 1 Hz refresh
void updateRunningDisplay() {
  unsigned long now = millis();
  if (now -lastRunningDisplay <RUNNING_DISPLAY_INTERVAL_MS) return;
  lastRunningDisplay =now;

  unsigned long elapsedMin =(now -breathStartMillis) /60000UL;

  lcd.clear();
  lcd.setCursor(0, 0);
  lcd.print("RUNNING "); lcd.print(elapsedMin); lcd.print("m");

  lcd.setCursor(0, 1);
  if (sessionEndMillis > 0) {
    unsigned long remainingMin = (sessionEndMillis > now) ?(sessionEndMillis - now)/60000UL :0;
    lcd.print(remainingMin); lcd.print("m left");
  } else {
    lcd.print("Press STOP");
  }
}

//  Setup
void setup() {
  pinMode(ENA1, OUTPUT);pinMode(DIR1, OUTPUT); pinMode(PUL1, OUTPUT);
  pinMode(ENA2, OUTPUT); pinMode(DIR2,OUTPUT); pinMode(PUL2,OUTPUT);
  pinMode(DIR_PIN, OUTPUT); pinMode(PWM_PIN,OUTPUT);
  pinMode(SYNC_PIN, OUTPUT);
  pinMode(RUN_PIN, OUTPUT);

  digitalWrite(ENA1, HIGH); //drivers disabled until a session actually starts
  digitalWrite(ENA2, HIGH);
  digitalWrite(DIR_PIN, HIGH);
  digitalWrite(SYNC_PIN, LOW);
  digitalWrite(RUN_PIN, LOW);
  analogWrite(PWM_PIN, 0);

  pinMode(SELECT_BUTTON, INPUT_PULLUP);
  pinMode(START_BUTTON, INPUT_PULLUP);
  pinMode(STOP_BUTTON, INPUT_PULLUP);
  pinMode(LIMIT_SW1, INPUT_PULLUP);
  pinMode(LIMIT_SW2, INPUT_PULLUP);

  STEPS_PER_MM = (MOTOR_STEPS_PER_REV* MICROSTEPPING) /RACK_TRAVEL_PER_REV_MM;

  C1 = RESP_A2;
  C2 = RESP_A1 -2.0 * RESP_A2* TAU_RS;
  C3 = RESP_A0 - RESP_A1 * TAU_RS +2.0 *RESP_A2 * TAU_RS * TAU_RS;
  EXH_DENOM = RRS * (1.0 / TAU_RS- 1.0 /TAU_EX);

  Serial.begin(9600);
  //Not necessary, just used for debugging 
  Serial.println("Comfort Mattress Controller Started");
  Serial.print("Pitch diameter (mm): "); Serial.println(GEAR_PITCH_DIAMETER_MM);
  Serial.print("Rack travel per rev (mm): "); Serial.println(RACK_TRAVEL_PER_REV_MM);
  Serial.print("STEPS_PER_MM: "); Serial.println(STEPS_PER_MM);

  lcd.init();
  lcd.backlight();
  lcd.clear();
  lcd.setCursor(0, 0);
  lcd.print("Comfort Bed");
  lcd.setCursor(0, 1);
  lcd.print("User Interface");
  delay(2000); // one-time blocking delay at power-up only

  systemState = SYS_CONFIG;
  displayMenu();
}

//  Main loop

void loop() {
  unsigned long now = millis();

  switch (systemState) {

    case SYS_CONFIG:
      readPotentiometer();
      readSelectButton();
      readStartButton();
      delay(20);
      break;

    case SYS_STARTING:
      updateStartingSequence();
      break;

    case SYS_RISING:
      updateRising();
      break;

    case SYS_RUNNING:
      readStopButton();

      if (now -lastBreathUpdate >=BREATH_UPDATE_INTERVAL_MS) {
        lastBreathUpdate = now;

        float elapsed_s= (now -breathStartMillis) /1000.0;
        float t_in_cycle = fmod(elapsed_s, T_CYCLE);

        float mm =baseHeight+amplitude* breathDisplacement(t_in_cycle);
        long targetSteps =lround(mm * STEPS_PER_MM);

        motor1.targetSteps = targetSteps;// dependent on wiring, can change sign i.e. -targetSteps
        motor2.targetSteps = targetSteps; // dependent on wiring, can change sign i.e. -targetSteps

        updateStepper(motor1);
        updateStepper(motor2);
      }

      updatePump();

      if (sessionEndMillis > 0 && now >= sessionEndMillis) {
        beginStopSequence();
      }

      updateRunningDisplay();
      break;

    case SYS_STOPPING:
      updateHoming();
      break;

    case SYS_DONE:
      readStartButton();
      break;
  }
}
