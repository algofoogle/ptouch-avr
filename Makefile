# Command used for assembling AVR assembly language files (*.asm):
ASM=avr-as

# Command for linking object files (*.o) to ELF binaries:
LINKER=avr-ld

# Command used for extracting binary data from ELF (*.elf) files:
OBJCOPY=avr-objcopy

# Device used for burning:
ISP=usbasp

# Device we're burning to (ATtiny13A):
DEVICE=t13

# AVRDUDE is used to interace with USBasp to burn to ATtiny13A:
BURN=avrdude -c $(ISP) -p $(DEVICE)

# Command for deleting stuff (e.g. during 'clean'):
ifndef HOME
  # HOME var not set... Let's assume Windows
  RM=del
else
  # Got a home directory, so let's assume Unix:
  RM=rm -f
endif

# Default build: Produce a binary file, and an Intel HEX file we can burn.
all:        ptouchavr.bin ptouchavr.hex

rebuild:	clean all

# Do clean, build ptouchavr.hex, and burn it.
rewrite:	clean ptouchavr.hex burn

# Burn ptouchavr.hex to an ATtiny13 using avrdude:
burn:       ptouchavr.hex
	$(BURN) -U flash:w:ptouchavr.hex:i

# Bring up the AVRDUDE terminal:
burnterm:
	$(BURN) -t

burntermf:
	$(BURN) -t -F

# Generate a HEX file that a burner can use:
ptouchavr.hex:	ptouchavr.elf
	$(OBJCOPY) --output-target=ihex ptouchavr.elf ptouchavr.hex

# Generate a compiled binary from the compiled ELF:
ptouchavr.bin:   ptouchavr.elf
	$(OBJCOPY) --output-target=binary ptouchavr.elf ptouchavr.bin

# Generate a compiled ELF from the Object file:
ptouchavr.elf:   ptouchavr.o
	$(LINKER) ptouchavr.o -o ptouchavr.elf

# Compile the assembly source to an object file:
ptouchavr.o:
	$(ASM) ptouchavr.asm -mmcu=attiny13a -o ptouchavr.o

# Delete any of our (possible) output files:
clean:
	$(RM) ptouchavr.bin ptouchavr.hex ptouchavr.elf ptouchavr.o

