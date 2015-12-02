Copyright (c) 2015 Hong Quach and Norah Alballa

This source file is licensed under the "MIT License." Please see the LICENSE
in this distribution for license terms.

# HaskellLED
Using Haskell to compute the animation to be rendered on the LED display using an Arduino and bit-shifts registers.  The LED display can be a matrix of LEDs, or a cube of LEDs, or any structure.

[Project Page & Documents] (http://htquach.github.io/HaskellLED/)

## LEDs Driver

This project is to drive a LED display using an Arduino UNO board and a set of bit-shift registers (e.g. 74HC595). The board is connected to a computer through the USB serial port and each of the frame to be display on the LED display is done so with the Haskell program. The role of the Arduino board is to receive a Hex encoded string where each bit represent an LED. For an 8x8 matrix LED, we would have 64 LED to render each frame. The data would be a Hex string of 64 bits or 16 Hex digits. The main driver is on the computer side written in Haskell to send to the board a new frame to be rendered.

## Getting Started

1. Build the LED Display
2. Upload the Arduino Sketch onto the Arduino board
3. Connect the board to the computer
4. Run main in the LEDDisplay module

## Haskell to Arduino Protocol

The initial attempt was using the existing Firmata protocol for this project. However, the refresh rate of the display demanded much higher rate than what can be achieved when trying to render the frame using the Firmata protocol. To resolve this, we created a simple protocol for sending the frame to the board and have the board render the frame directly there.

The Arduino board is set to listen on its serial port for new frame data to be render. The frame data format is a Hex encded string of the 1s and 0s bits that map to the ON or OFF state of each of the LED. The length and order of the bits depend on the number and order of wiring of the bit-shift registers.
