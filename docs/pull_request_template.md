# Description

Please include a summary of the change and which issue is fixed. Please also include relevant motivation and context. List any dependencies that are required for this change. Refer any relevant issues and / or board tag.

Fixes # (issue)

## Type of change

Please delete options that are not relevant.

- [ ] Bug fix (non-breaking change which fixes an issue)
- [ ] New feature (non-breaking change which adds functionality)
- [ ] Breaking change (fix or feature that would cause existing functionality to not work as expected)
- [ ] This change requires a documentation update

# How Has This Been Tested?

Please describe the tests that you ran to verify your changes. Provide instructions so we can reproduce. Please also list any relevant details for your test configuration

- [ ] Test A
- [ ] Test B

## Default core tests

- [ ] The synthesiser shall play the appropriate musical tone as a sawtooth wave when a key is pressed
- [ ] There shall be no perceptible delay between pressing a key and the tone starting
- [ ] There shall be a volume control with at least 8 increments, which shall be operated by turning a knob
- [ ] The OLED display shall show the name of the note being played and the current volume level
- [ ] The synthesiser shall send a message on the serial port whenever a key is pressed or released
- [ ] The synthesiser shall play a note or stop playing a note when it receives an appropriate message on the serial port
- [ ] Serial port messages for playing a note are functional
- [ ] Serial port messages for ending a note are functional

**Test Configuration**:
* Operating System:

# Checklist:

- [ ] My code follows the style guidelines of this project
- [ ] I have performed a self-review of my own code
- [ ] I have commented my code, particularly in hard-to-understand areas
- [ ] I have made corresponding changes to the documentation
- [ ] My changes generate no new warnings
- [ ] I have added tests that prove my fix is effective or that my feature works
- [ ] New and existing unit tests pass locally with my changes
- [ ] Any dependent changes have been merged and published in downstream modules
- [ ] This pull request has been reviewed by another team member