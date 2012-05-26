# Cuis-ProfStef #

ProfStef port to Cuis Smalltalk.

Ported packages are:
* Cuis-ProfStef-Core
* Cuis-ProfStef-Tests

## Usage ##

Load packages into a Cuis image, open a Workspace and type

	ProfStef go.

select and evaluate the typed text then follow on screen instructions to learn Smalltalk.

## Release Notes ##

This is my first attempt in porting code to Cuis.
Suggestions are welcome on:

* LessonEditor class - currently used to display ProfStef menu (maybe not needed). ProfStef instance singleton has been used to initialize the LessonEditor

* SmalltalkSyntaxTutorial>>instanciation - because SimpleButtonMorph class is not available in Cuis I have used EllipseMorph, maybe not the most effective alternative

* SmalltalkSyntaxTutorial>>reflection - I was not able to find the code to retrieve method comment from a CompiledMethod instance


