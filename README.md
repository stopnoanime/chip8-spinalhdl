# CHIP-8 implementation made in SpinalHDL

This project was made for the Lichee Tang FPGA board but it should by easy to adapt to any other board.

This project uses [this](https://github.com/ibm2030/SimpleSDHC) SD card controller and [this](https://github.com/lawrie/VexRiscv/blob/master/src/main/scala/vexriscv/demo/PS2Keyboard.scala) PS2 controller

Roms are stored on a unformated SD card. Go to the roms folder to learn more about this.

Demonstration video available [here](https://youtu.be/M-VnHZBb9W4).

Go [here](https://en.wikipedia.org/wiki/CHIP-8) to learn more about CHIP-8.

## Required external interfaces:
1. VGA 
2. PS2 keyboard
3. SD card for storing roms


## Kyboard mappings:
CHIP-8 hex keypad:
<table>
    <tr>
        <td>1</td>
        <td>2</td>
        <td>3</td>
        <td>4</td>
    </tr>
    <tr>
        <td>Q</td>
        <td>W</td>
        <td>E</td>
        <td>R</td>
    </tr>
    <tr>
        <td>A</td>
        <td>S</td>
        <td>D</td>
        <td>F</td>
    </tr>
    <tr>
        <td>Z</td>
        <td>X</td>
        <td>C</td>
        <td>V   </td>
    </tr>
</table>
<br>

Programmer control:
<table>
    <tr>
        <td>Load program</td><td>L</td>
    </tr>
    <tr>
        <td>Change screen color</td><td>K</td>
    </tr>
    <tr>
        <td>Go to next program</td><td>></td>
    </tr>
    <tr>
        <td>Go to last program</td><td><</td>
    </tr>
</table>
<br>

