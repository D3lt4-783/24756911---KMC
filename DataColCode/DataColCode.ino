// Pins
const int sensorPin = A0;
const int potPin = A1;
const int PWM_PIN = 5;
const int DIR_PIN = 8;

// Baseline
int baseline = 0;

// Timing
unsigned long lastBeatTime = 0;

void setup() {
  Serial.begin(115200);

  pinMode(PWM_PIN, OUTPUT);
  pinMode(DIR_PIN, OUTPUT);
  digitalWrite(DIR_PIN, HIGH);

  // Baseline measurement
  for (int i = 0; i < 100; i++) {
    baseline += analogRead(sensorPin);
    delay(5);
  }
  baseline /= 100;

  // CSV Header
  Serial.println("time_ms,vibration,bpm");

  Serial.println("System started");
}

void loop() {

  // --- Read BPM from potentiometer ---
  int potValue = analogRead(potPin);
  int bpm = map(potValue, 0, 1023, 40, 120);

  int period = 60000 / bpm;       
  int onTime = max(120, period * 0.12);
  int offTime = period - onTime;

  unsigned long currentTime = millis();

  // --- Pulse logic ---
  if (currentTime - lastBeatTime >= period) {
    
    analogWrite(PWM_PIN, 128);
    delay(onTime);

    analogWrite(PWM_PIN, 0);

    lastBeatTime = millis();
  }

  // --- Read vibration ---
  int val = analogRead(sensorPin);
  int vibration = abs(val - baseline);

  // --- Output as CSV ---
  Serial.print(millis());   // timestamp
  Serial.print(",");
  Serial.print(vibration);
  Serial.print(",");
  Serial.println(bpm);

  delay(1);
}