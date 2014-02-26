# Brother P-Touch Hacking with AVR


## What is this?

This is *work-in-progress* AVR Assembly source code that is designed to drive the
TPH (Thermal Print Head) of a Brother P-Touch label printer, to theoretically print
any graphics.

I am writing this code to target an Atmel ATtiny13A, to control the TPH of a
Brother P-Touch PT-1010 hand-held label printer. I am also adding conditional
compilation to the code so that it can target the ATtiny25/45/85. This code is
expected to also drive the print head of a PT-1000.

In time this repo will accompany an article that I will later post on my blog:
<http://anton.maurovic.com/>

This work is based, in part, on an
[article on philpem's blog](http://blog.philpem.me.uk/?p=171).



## Disclaimer

NOTE that this code is currently just a proof-of-concept and may not work properly,
and at this time is **NOT CONSIDERED FIT FOR PUBLIC RELEASE OR USE**. Use it at
your own risk; timing errors or other bad wiring may lead to damaging either your
Brother P-Touch printer control circuitry or (more likely) its TPH.




## How do I load the firmware and test the hardware?

### Building the firmware

I installed [WinAVR](http://winavr.sourceforge.net/download.html) on Windows, which installs
`avr-gcc` (including the `avr-as` assembler) and `avrdude` for interfacing with a USBasp
to write the firmware to an AVR MCU.

On a Mac, you can use Homebrew to install [Lars Immisch's `avr-gcc` fork](https://github.com/larsimmisch/homebrew-avr):

    brew tap larsimmisch/avr
    brew install avr-libc

After that you should be able to build the `.hex` file with:

    make rebuild


### Burning the firmware to the MCU's Flash ROM

See [the burning instructions](https://github.com/algofoogle/tests/tree/master/avr/05#how-do-i-load-the-firmware-and-test-the-hardware)
in earlier examples, **including help
[if you have problems](https://github.com/algofoogle/tests/tree/master/avr/05#if-you-have-problems-burning-the-firmware).**



## Who wrote this?

This was written by [Anton Maurovic](http://anton.maurovic.com). You can use it
for whatever you like, but giving me credit with a link to <http://anton.maurovic.com>
would be appreciated!
