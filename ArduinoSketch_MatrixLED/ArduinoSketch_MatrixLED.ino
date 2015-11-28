/**
 * Copyright (c) 2015 Hong Quach
 *
 * This source file is licensed under the "MIT License." Please see the LICENSE
 * in this distribution for license terms.
 *
 * Matrix rendering controlled by serial
 * Send each frame through the serial port (64 bytes buffer size)
 * marked with the startChar and end with the endChar.
 *
 * Author:  hong dot t dot quach at g m a i l dot com
 */

String inputString = "";         // a string to hold incoming data
boolean stringComplete = false;  // whether the string is complete
char startChar = '^';            // char to indicate the start of matrix data
char endChar = '$';              // char to indicate the end of the matrix data
boolean dataStarted = false;     // signal that matrix data is comming in

// Initial frame "{The Haskell Logo} LED"
String matrixData = "fffef823fffefbedb7fefbeedb0ef86eedfefbeedacefbedb77e0823ffffffff";
String newMatrixData = "";

boolean enableDebug = false;
int matrixWidth = 32;
int matrixHeight = 8;
int serialSpeed = 9600;

//Pin connected to latch pin #12 (ST_CP) of 74HC595
const int latchPin = 12;
//Pin connected to clock pin #11 (SH_CP) of 74HC595
const int clockPin = 11;
//Pin connected to Data pin #14 (DS) of 74HC595
const int dataPin = 4;
//Pin connect to LED for visual feedback for debug purpose
const int ledPin = 13;

/**
 * Arduino init function
 */
void setup() {
  //set pins to output because they are addressed in the main loop
  pinMode(latchPin, OUTPUT);
  pinMode(dataPin, OUTPUT);
  pinMode(clockPin, OUTPUT);
  Serial.begin(serialSpeed);
  Serial.print("Starting Listing on Serial Port.  ");
  Serial.println("Use '^' to start a new data and '$' to terminate.");
  // reserve 200 bytes for strings:
  matrixData.reserve(200);
  newMatrixData.reserve(200);
  //TODO:  Send out the startChar, endChar, matrix Width and Height to the Host
}

/**
 * Recieve chars from serial port
 */
void getSerialInput() {
  for (int i = 0; i < 66; i++) {
    if (Serial.available()) {
      char inChar = (char)Serial.read();
      if (inChar == startChar && !dataStarted) {
        if (enableDebug) {
          digitalWrite(ledPin, true);
          Serial.println("Start char detected");
        }
        newMatrixData = "";
        dataStarted = true;
      }
      else if (inChar == endChar && dataStarted) {
        if (enableDebug) {
          Serial.println("End   char detected");
        }
        if (newMatrixData == "^enableDebug^") {
          enableDebug = true;
        }
        else if (newMatrixData == "^disableDebug^") {
          enableDebug = false;
        }
        else {
          matrixData = newMatrixData;
          dataStarted = false;
        }
        if (enableDebug) {
          digitalWrite(ledPin, false);
          Serial.print("New Frame:  ");
          Serial.println(matrixData);
        }
      }
      else if (dataStarted) {
        newMatrixData += inChar;
      }
    }
    else {
      break;
    }
  }
}

/**
 * The 'main' of Arduino
 */
void loop() {
  getSerialInput();
  render(matrixData, 1);
}

/**
 * Render each frame
 */
void render(String frame, int holdCycle) {
  int registerCount = 5;
  int layerCount = 8;
  byte registers[registerCount];
  for (int h = 0; h < holdCycle; h++) {
    unsigned layerBit = 1;
    for (int layer = 0; layer < layerCount; layer++) {
      // Lines up the bits to be shipped to the register
      int subFrameIndex = layer * layerCount;
      registers[0] = byte(layerBit);
      for (int r = 1; r < registerCount; r++)
      {
        registers[r] = hexStringToByte(frame.substring(subFrameIndex + ((r - 1) * 2), subFrameIndex + (r * 2)));
      }
      shiftOutToRegisters(registerCount, registers);

      //Next layer
      layerBit = (layerBit << 1);
    }
  }
}

/**
 * shifting one byte per register (74HC595) wired in series.
 */
void shiftOutToRegisters(int registerCount, byte* registers) {
  // turn off the output so the pins don't light up
  // while you're shifting bits:
  digitalWrite(latchPin, LOW);
  for (int i = 0; i < registerCount; i++) {
    shiftOut(dataPin, clockPin, MSBFIRST, registers[i]);
  }
  // turn on the output so the LEDs can light up:
  digitalWrite(latchPin, HIGH);
}

/**
 * Convert a string in HEX to a byte
 */
byte hexStringToByte(String hexString) {
  byte temp = 0;
  int input_len = hexString.length();
  hexString.toUpperCase();
  for (int i = 0; i < input_len; i++) {
    int int_of_char = int(hexString[i]);
    int int_of_0 = int('0');
    int int_of_A = int('A');
    int char_value = 0;
    if (int_of_char >= int_of_0 && int_of_char <= int_of_0 + 9) {
      char_value = int_of_char - int_of_0;
    } else if (int_of_char >= int_of_A && int_of_char <= int_of_A + 5) {
      char_value = int_of_char - int_of_A + 10;
    }
    temp |= (char_value << ((input_len - 1 - i) * 4));
  }
  return temp;
}

