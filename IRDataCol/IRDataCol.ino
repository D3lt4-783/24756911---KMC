const int sensorPin = A4;
const unsigned long RECORD_DURATION = 500000UL; // 500 seconds in milliseconds

unsigned long startTime;
bool recording = true;

void setup() {
  Serial.begin(9600);
  
  // Print CSV Header to identify the columns
  Serial.println("Timestamp(ms),Raw_ADC,Voltage(V),Distance(mm)");

  startTime = millis();
}

void loop() {

  if (!recording) {
    return; // Stop doing anything once recording has ended
  }

  unsigned long timeStamp = millis();

  // Check if 500 seconds have elapsed
  if (timeStamp - startTime >= RECORD_DURATION) {
    recording = false;
    Serial.println("Recording stopped: 500 seconds elapsed.");
    return;
  }

  int raw = analogRead(sensorPin);
  float voltage = raw * (5.0 / 1023.0);

  if (voltage < 0.1) voltage = 0.1;
 
  float distanceCM = 13 * pow(voltage, -1);

  float distanceMM = distanceCM * 10.0;
  distanceMM = constrain(distanceMM, 40, 300);

  // 6. Print CSV Row (Data separated by commas)
  Serial.print(timeStamp);
  Serial.print(",");
  Serial.print(raw);
  Serial.print(",");
  Serial.print(voltage, 2); // 2 decimal places
  Serial.print(",");
  Serial.println(distanceMM, 0); // 0 decimal places, ends line

  delay(200);
}