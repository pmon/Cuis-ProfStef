'From Cuis 4.0 of 21 April 2012 [latest update: #1291] on 26 May 2012 at 11:32:05 pm'!
'Description Please enter a description for this package '!
!classDefinition: #AbstractTutorial category: #'Cuis-ProfStef-Core'!
Object subclass: #AbstractTutorial
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-ProfStef-Core'!
!classDefinition: 'AbstractTutorial class' category: #'Cuis-ProfStef-Core'!
AbstractTutorial class
	instanceVariableNames: ''!

!classDefinition: #HowToMakeYourOwnTutorial category: #'Cuis-ProfStef-Core'!
AbstractTutorial subclass: #HowToMakeYourOwnTutorial
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-ProfStef-Core'!
!classDefinition: 'HowToMakeYourOwnTutorial class' category: #'Cuis-ProfStef-Core'!
HowToMakeYourOwnTutorial class
	instanceVariableNames: ''!

!classDefinition: #Lesson category: #'Cuis-ProfStef-Core'!
Object subclass: #Lesson
	instanceVariableNames: 'title lesson'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-ProfStef-Core'!
!classDefinition: 'Lesson class' category: #'Cuis-ProfStef-Core'!
Lesson class
	instanceVariableNames: ''!

!classDefinition: #LessonEditor category: #'Cuis-ProfStef-Core'!
SmalltalkEditor subclass: #LessonEditor
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-ProfStef-Core'!
!classDefinition: 'LessonEditor class' category: #'Cuis-ProfStef-Core'!
LessonEditor class
	instanceVariableNames: ''!

!classDefinition: #LessonView category: #'Cuis-ProfStef-Core'!
Object subclass: #LessonView
	instanceVariableNames: 'shoutMorph window lesson'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-ProfStef-Core'!
!classDefinition: 'LessonView class' category: #'Cuis-ProfStef-Core'!
LessonView class
	instanceVariableNames: ''!

!classDefinition: #ProfStef category: #'Cuis-ProfStef-Core'!
Object subclass: #ProfStef
	instanceVariableNames: 'lessonView player'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-ProfStef-Core'!
!classDefinition: 'ProfStef class' category: #'Cuis-ProfStef-Core'!
ProfStef class
	instanceVariableNames: 'instance'!

!classDefinition: #SmalltalkSyntaxTutorial category: #'Cuis-ProfStef-Core'!
AbstractTutorial subclass: #SmalltalkSyntaxTutorial
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-ProfStef-Core'!
!classDefinition: 'SmalltalkSyntaxTutorial class' category: #'Cuis-ProfStef-Core'!
SmalltalkSyntaxTutorial class
	instanceVariableNames: ''!

!classDefinition: #TutorialPlayer category: #'Cuis-ProfStef-Core'!
Object subclass: #TutorialPlayer
	instanceVariableNames: 'tutorialPosition tutorial'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-ProfStef-Core'!
!classDefinition: 'TutorialPlayer class' category: #'Cuis-ProfStef-Core'!
TutorialPlayer class
	instanceVariableNames: ''!


!AbstractTutorial commentStamp: 'pm 5/21/2012 22:55' prior: 0!
Parent class of all ProfStef tutorials.

To create your own tutorial:
- subclass AbstractTutorial
- implement a few methods which returns a Lesson instance
- implement tutorial which returns a Collection of selectors to the methods you've created.

For example, see MockTutorial (minimalist) and SmalltalkSyntaxTutorial (default ProfStef one).

See ProfStef comment to execute your own tutorial.!

!HowToMakeYourOwnTutorial commentStamp: 'pmon 5/21/2012 00:04' prior: 0!
I'm a ProfStef tutorial which teach to create a ProfStef tutorial. Open me with

ProfStef goOn: HowToMakeYourOwnTutorial.!

!Lesson commentStamp: 'pmon 5/20/2012 16:22' prior: 0!
See class side messages #welcome, #doingVSPrinting....
!

!LessonEditor commentStamp: 'pm 5/21/2012 22:57' prior: 0!
Menu for lesson models require a stripped down version of SmalltalkEditor, not sure this is the best approach ... !

!LessonView commentStamp: 'LaurentLaffont 1/15/2010 10:24' prior: 0!
A LessonView displays a Lesson instance!

!ProfStef commentStamp: 'pmon 5/20/2012 16:14' prior: 0!
A ProfStef is the Smalltalk teacher. To start the tutorial, evaluate:
ProfStef go.

To go to the next lesson evaluate:
ProfStef next.

To execute your own tutorial:
ProfStef goOn: MyOwnTutorial

To see a table of contents with all defined tutorials:
ProfStef contents!

!SmalltalkSyntaxTutorial commentStamp: 'LaurentLaffont 1/21/2010 16:50' prior: 0!
The default ProfStef tutorial to learn Smalltalk syntax!

!TutorialPlayer commentStamp: 'LaurentLaffont 1/21/2010 20:34' prior: 0!
I can navigate through an AbstractTutorial subclass. With #next and #previous you can go forward and backward through the tutorial. !

!AbstractTutorial methodsFor: 'tutorial' stamp: 'pm 5/21/2012 22:57'!
indexOfLesson: aSelector
	^self tutorial indexOf: aSelector.! !

!AbstractTutorial methodsFor: 'tutorial' stamp: 'pm 5/21/2012 22:57'!
lessonAt: anInteger
	| lessonSelector |
	lessonSelector := self tutorial at: anInteger.
	^ self perform: lessonSelector.! !

!AbstractTutorial methodsFor: 'accessing' stamp: 'pm 5/21/2012 22:57'!
lessons
	^ self tutorial collect: [:aSelector| self perform: aSelector]! !

!AbstractTutorial methodsFor: 'printing' stamp: 'pm 5/21/2012 22:57'!
printOn: aStream
	aStream 
		nextPutAll: 'a ProfStef Tutorial (';
		nextPutAll: self class title;
		nextPutAll: ')'.    ! !

!AbstractTutorial methodsFor: 'accessing' stamp: 'pm 5/21/2012 22:57'!
size
	^ self tutorial size! !

!AbstractTutorial methodsFor: 'tutorial' stamp: 'pm 5/21/2012 22:57'!
tutorial
	"Should return an Array of selectors which returns Lesson instances.
	See SmalltalkSyntaxTutorial."
	^ self shouldBeImplemented.! !

!AbstractTutorial class methodsFor: 'tutorial metainfo' stamp: 'pmon 5/21/2012 23:20'!
title	
	"Return the title of the tutorial by parsing the class name 	
	like a Wiki link and interspersing whitespaces between the tokens"	
	| className separators groups |	
			
	className := self name.	
	separators := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.	
	groups := className findTokens: separators keep: separators.	
	^ (groups pairsCollect: [ :sep :rest | sep , rest ]) collect: [:each | each] andFold: [:a :b | a , ' ' , b].
	! !

!AbstractTutorial class methodsFor: 'tutorial metainfo' stamp: 'pmon 5/26/2012 22:25'!
tutorials
	^ (self subclasses 
		sort: [:a :b |
			   a name < b name]) select: [:aTutorial |	
				                                      (aTutorial category = 'Cuis-ProfStef-Tests') not ].
! !

!HowToMakeYourOwnTutorial methodsFor: 'lessons' stamp: 'pmon 5/21/2012 00:07'!
createLessonFactoryMethods
	^ Lesson 
title: 'Lesson factory methods'
lesson: 
'"Next, create category ''lessons'' and add a method per lesson.

Each method must return a Lesson object.

Your can use Lesson class>>title:lesson: to create Lesson object."

HowToDebug compile:
''useSelfHalt
	^ Lesson
title: ''''self halt''''
lesson: 
''''"Put self halt. in the method you want to debug."

ProfStef next.''''''
classified: ''lessons''.

HowToDebug compile:
''examineStackTrace
	^ Lesson
title: ''''self halt''''
lesson: 
''''"Look at PharoDebug.log."

ProfStef next.''''''
classified: ''lessons''.

HowToDebug compile:
''changeReturnValue
	^ Lesson
title: ''''Change return value''''
lesson: 
''''"Easy in the debugger !!"''''''
classified: ''lessons''.

ProfStef next.'.! !

!HowToMakeYourOwnTutorial methodsFor: 'lessons' stamp: 'pmon 5/21/2012 00:07'!
implementTutorial
	^ Lesson 
title: 'Implement tutorial method'
lesson: 
'"Finally implement the tutorial method to return an Array of your lesson factory methods:"

HowToDebug compile:
''tutorial
	^ #(
#useSelfHalt
#examineStackTrace
#changeReturnValue
)''
classified: ''tutorial''.


ProfStef next.'.! !

!HowToMakeYourOwnTutorial methodsFor: 'lessons' stamp: 'pmon 5/21/2012 00:08'!
runYourTutorial
	^ Lesson 
title: 'Run your tutorial'
lesson: 
'"You can run your fresh new tutorial like this:" 

ProfStef goOn: HowToDebug.'.! !

!HowToMakeYourOwnTutorial methodsFor: 'lessons' stamp: 'pmon 5/21/2012 00:08'!
subclassAbstractTutorial
	^ Lesson 
title: 'AbstractTutorial'
lesson: 
'"Here are the steps to create your own ProfStef tutorial. 

First, create a subclass of AbstractTutorial. For example:"

AbstractTutorial subclass: #HowToDebug
	instanceVariableNames: ''''
	classVariableNames: ''''
	poolDictionaries: ''''
	category: ''ProfStef''.
	
ProfStef next.'! !

!HowToMakeYourOwnTutorial methodsFor: 'tutorial' stamp: 'pmon 5/21/2012 00:05'!
tutorial
	^ #(
subclassAbstractTutorial
createLessonFactoryMethods
implementTutorial
runYourTutorial
)! !

!Lesson methodsFor: 'accessing' stamp: 'pmon 5/20/2012 16:22'!
lesson
	^ lesson ifNil: [lesson := '']! !

!Lesson methodsFor: 'accessing' stamp: 'pmon 5/20/2012 16:22'!
lesson: aString
	lesson := aString! !

!Lesson methodsFor: 'printing' stamp: 'pmon 5/20/2012 16:22'!
printOn: aStream
	super printOn: aStream. 
	aStream 
		nextPutAll: '(';
		nextPutAll: self title;
		nextPutAll: ')'.    ! !

!Lesson methodsFor: 'accessing' stamp: 'pmon 5/20/2012 16:22'!
title
	^ title ifNil: [title := '']! !

!Lesson methodsFor: 'accessing' stamp: 'pmon 5/20/2012 16:22'!
title: aString
	title := aString! !

!Lesson class methodsFor: 'instance creation' stamp: 'pmon 5/20/2012 16:22'!
title: aStringForTitle lesson: aStringForLesson
	^ self new title: aStringForTitle; lesson: aStringForLesson; yourself.! !

!LessonEditor class methodsFor: 'as yet unclassified' stamp: 'pmon 5/20/2012 22:14'!
initializeMenu
	"Initialize the mouseButton2 (right button) pop-up menu and corresponding messages."

	"Editor initialize"

	menu _ SelectionMenu fromArray: {
"		{'Undo - multiple (z)'.			#undo}.
		{'Redo - multiple (Z)'.			#redo}.
		{'Undo / Redo history'.			#offerUndoHistory}.
		#-.
		{'Copy (c)'.						#copySelection}.
		{'Cut (x)'.						#cut}.
		{'Paste (v)'.						#paste}.
		{'Paste without Format'.		#pasteString}.
		{'Paste...'.						#pasteRecent}.
		#-.
"		{'Do it (d)'.						#doIt}.
		{'Print it (p)'.					#printIt}.
"		{'Inspect it (i)'.					#inspectIt}.
		{'Explore it (I)'.					#exploreIt}.
		{'Debug it'.						#debugIt}.
"	}! !

!LessonView methodsFor: 'gui' stamp: 'pmon 5/20/2012 16:18'!
close
	self window delete.! !

!LessonView methodsFor: 'as yet unclassified' stamp: 'pmon 5/20/2012 17:38'!
editorClass
	^LessonEditor! !

!LessonView methodsFor: 'as yet unclassified' stamp: 'pmon 5/20/2012 18:02'!
is: aSymbol
	^ (aSymbol == #ShoutEnabled)! !

!LessonView methodsFor: 'as yet unclassified' stamp: 'pmon 5/20/2012 17:22'!
lessonText
	^ lesson
		ifNil: [ '' ]
		ifNotNil: [ lesson lesson ]! !

!LessonView methodsFor: 'gui' stamp: 'pm 5/21/2012 23:03'!
open	
	"shoutMorph  := PluggableTextMorph 
		on: self 
		text: nil
		accept: nil
		readSelection: nil 
		menu: #shoutMorphFillMenu:.
	shoutMorph
		setText: ''.
	"
	shoutMorph := TextModelMorph textProvider: self textGetter: #lessonText.
	
	window  := SystemWindow new model: shoutMorph .
	window setLabel: 'PrStef lesson'.
	window layoutMorph addMorphUseAll: shoutMorph .
	window openInWorld.! !

!LessonView methodsFor: 'accessing' stamp: 'pmon 5/20/2012 16:35'!
shoutAboutToStyle: aPluggableShoutMorphOrView
	^ true! !

!LessonView methodsFor: 'accessing' stamp: 'pmon 5/20/2012 16:35'!
shoutMorph
	^ shoutMorph ifNil: [self open. shoutMorph]! !

!LessonView methodsFor: 'gui' stamp: 'pmon 5/26/2012 22:38'!
showLesson: aLesson withTitle: aString
	self window setLabel: aString.
	lesson := aLesson.
	self shoutMorph model: ( (PluggableTextModel on: self) textGetter: #lessonText textSetter: nil selectionGetter: nil ).

	World findAWindowSatisfying: [ :window | window = self window ] orMakeOneUsing: [ self window openInWorld ].
	! !

!LessonView methodsFor: 'accessing' stamp: 'pmon 5/20/2012 16:36'!
text
	^ self shoutMorph text asString! !

!LessonView methodsFor: 'accessing' stamp: 'pmon 5/20/2012 17:23'!
window 
	^ window ifNil: [self open. window]! !

!ProfStef methodsFor: 'gui' stamp: 'pmon 5/20/2012 16:15'!
close
	self lessonView close! !

!ProfStef methodsFor: 'navigating' stamp: 'pmon 5/20/2012 16:16'!
first
	self player first.
	^ self showCurrentLesson.! !

!ProfStef methodsFor: 'starting' stamp: 'pmon 5/20/2012 16:17'!
go
	^ self goOn: SmalltalkSyntaxTutorial.! !

!ProfStef methodsFor: 'starting' stamp: 'pmon 5/20/2012 16:18'!
goOn: aTutorialClass
	self player tutorial: aTutorialClass new.
	^ self open.! !

!ProfStef methodsFor: 'navigating' stamp: 'pmon 5/20/2012 16:17'!
last
	self player last.
	^ self showCurrentLesson.! !

!ProfStef methodsFor: 'accessing' stamp: 'pmon 5/20/2012 16:18'!
lessonView
	^ lessonView ifNil: [lessonView := LessonView new]! !

!ProfStef methodsFor: 'accessing' stamp: 'pmon 5/20/2012 16:18'!
lessonView: aLessonView
	lessonView := aLessonView.! !

!ProfStef methodsFor: 'navigating' stamp: 'pmon 5/20/2012 16:17'!
next
	self player next.
	^ self showCurrentLesson.! !

!ProfStef methodsFor: 'gui' stamp: 'pmon 5/20/2012 16:15'!
open
	self player first.
	^ self showCurrentLesson.! !

!ProfStef methodsFor: 'accessing' stamp: 'pmon 5/20/2012 16:18'!
player 
	^ player ifNil: [player := TutorialPlayer new]! !

!ProfStef methodsFor: 'accessing' stamp: 'pmon 5/20/2012 16:18'!
player: aTutorialPlayer
	player := aTutorialPlayer.! !

!ProfStef methodsFor: 'navigating' stamp: 'pmon 5/20/2012 16:17'!
previous
	self player previous.
	^ self showCurrentLesson.! !

!ProfStef methodsFor: 'gui' stamp: 'pmon 5/20/2012 16:16'!
showCurrentLesson
	| progressInfo lesson |
	lesson := self player currentLesson.
	progressInfo := '(', self tutorialPositionString, '/', self tutorialSizeString, ')'. 
	^ self lessonView   showLesson: lesson withTitle: lesson title, ' ', progressInfo.! !

!ProfStef methodsFor: 'navigating' stamp: 'pmon 5/20/2012 16:17'!
tutorial: aTutorialClass lesson: aSelector
	| tutorial |
	tutorial := aTutorialClass new.
	self player tutorial: tutorial.
	self tutorial: aTutorialClass lessonAt: (tutorial indexOfLesson: aSelector).! !

!ProfStef methodsFor: 'navigating' stamp: 'pmon 5/20/2012 16:17'!
tutorial: aTutorialClass lessonAt: lessonIndex
	self player tutorial: aTutorialClass new.
	self player tutorialPosition: lessonIndex.
	self showCurrentLesson.! !

!ProfStef methodsFor: 'accessing' stamp: 'pmon 5/20/2012 16:18'!
tutorialPositionString
	^ player tutorialPosition asString.! !

!ProfStef methodsFor: 'accessing' stamp: 'pmon 5/20/2012 16:18'!
tutorialSizeString
	^ self player size asString! !

!ProfStef class methodsFor: 'class initialization' stamp: 'pm 5/21/2012 22:54'!
default 
	^ instance ifNil: [
		Editor initialize. "required to initialize class instaces of LessonEditor class, maybe there are better place to do this ?"
		instance := self new]! !

!ProfStef class methodsFor: 'navigating' stamp: 'pmon 5/20/2012 16:14'!
first
	^ self default first.! !

!ProfStef class methodsFor: 'navigating' stamp: 'pmon 5/20/2012 16:14'!
go
	^ self default go.! !

!ProfStef class methodsFor: 'navigating' stamp: 'pmon 5/20/2012 16:15'!
goOn: aTutorialClass
	^ self default goOn: aTutorialClass.! !

!ProfStef class methodsFor: 'navigating' stamp: 'pmon 5/20/2012 16:15'!
last
	^ self default last.! !

!ProfStef class methodsFor: 'navigating' stamp: 'pmon 5/20/2012 16:15'!
next
	^ self default next.! !

!ProfStef class methodsFor: 'navigating' stamp: 'pmon 5/20/2012 16:15'!
previous
	^ self default previous.! !

!ProfStef class methodsFor: 'class initialization' stamp: 'pmon 5/20/2012 16:14'!
reset 
	instance := nil! !

!ProfStef class methodsFor: 'starting' stamp: 'pmon 5/20/2012 16:15'!
tutorial: aTutorialClass lesson: aSelector
	self default tutorial: aTutorialClass lesson: aSelector.! !

!ProfStef class methodsFor: 'starting' stamp: 'pmon 5/20/2012 16:15'!
tutorial: aTutorialClass lessonAt: lessonIndex
	self default tutorial: aTutorialClass lessonAt: lessonIndex.! !

!SmalltalkSyntaxTutorial methodsFor: 'lessons' stamp: 'pmon 5/20/2012 22:32'!
basicTypesArray
	^ Lesson
title: 'Basic types: Array' 
lesson: 
'"Literal arrays are created at parse time:"

#(1 2 3).

#( 1 2 3 #(4 5 6)) size.

#(1 2 4) isEmpty.

#(1 2 3) first.

#(''hello'' ''Squeak'') at: 2 put: ''Cuis''; yourself.

ProfStef next.'! !

!SmalltalkSyntaxTutorial methodsFor: 'lessons' stamp: 'pmon 5/20/2012 22:25'!
basicTypesCharacters
	^ Lesson
title: 'Basic types: Characters' 
lesson: 
'"A Character can be instantiated using $ operator:"

$A.

$A class.

$B asciiValue.

Character crCharacter.

Character space.

"You can print all 256 characters of the ASCII extended set:"

Character allCharacters.

ProfStef next.'! !

!SmalltalkSyntaxTutorial methodsFor: 'lessons' stamp: 'pmon 5/20/2012 22:34'!
basicTypesDynamicArray
	^ Lesson
title: 'Basic types: Dynamic Array' 
lesson: 
'"Dynamic Arrays are created at execution time:"

{ (2+3) . (6*6) }.

{ (2+3) . (6*6) . ''hello'', '' Stef''} size.


{ ProfStef } first next.'! !

!SmalltalkSyntaxTutorial methodsFor: 'lessons' stamp: 'pmon 5/20/2012 22:17'!
basicTypesNumbers
	^ Lesson
title: 'Basic types: Numbers' 
lesson: 
'"You now know how to execute Smalltalk code. 

Now let''s talk about basic objects.

1, 2, 100, 2/3 ... are Numbers, and respond to many messages evaluating mathematical expressions.
Evaluate these ones:"

2.

20 factorial.

1000 factorial / 999 factorial.

(1/3).

(1/3) + (4/5).

(1/3) asFloat.

1 class.

1 class maxVal class.

(1 class maxVal + 1) class.

ProfStef next.'! !

!SmalltalkSyntaxTutorial methodsFor: 'lessons' stamp: 'pmon 5/20/2012 22:19'!
basicTypesString
	^ Lesson
title: 'Basic types: Strings' 
lesson: 
'"A String is a collection of characters. Use single quotes to create a String object. Print these expressions:"

''ProfStef''.

''ProfStef'' size.

''abc'' asUppercase.

''Hello World'' reverse. 

"You can access each character using at: message"

''ProfStef'' at: 1.

"String concatenation uses the comma operator:"

''ProfStef'', '' is cool''.

ProfStef next.'! !

!SmalltalkSyntaxTutorial methodsFor: 'lessons' stamp: 'pmon 5/20/2012 22:28'!
basicTypesSymbol
	^ Lesson
title: 'Basic types: Symbols' 
lesson: 
'"A Symbol is a String which is guaranteed to be globally unique. 

There is one and only one Symbol #ProfStef. There may be several ''ProfStef'' String objects.

(Message == returns true if the two objects are the SAME)"

''ProfStef'' asSymbol.

#ProfStef asString.

(2 asString) == (2 asString).

(2 asString) asSymbol == (2 asString) asSymbol.


(Smalltalk at: #ProfStef) next.'! !

!SmalltalkSyntaxTutorial methodsFor: 'lessons' stamp: 'pmon 5/20/2012 23:07'!
blocks
	^ Lesson
title: 'Blocks' 
lesson: 
'"Cascade is cool !! Let''s talk about blocks.

Blocks are anonymous methods that can be stored into variables and executed on demand.

Blocks are delimited by square brackets: []"

[BrowserWindow openBrowser].

"does not open a Browser because the block is not executed.

Here is a block that adds 2 to its argument (its argument is named x):"

[:x | x+2].

"We can execute a block by sending it value messages."

[:x | x+2] value: 5.

[BrowserWindow openBrowser] value.

[:x | x+2] value: 10.

[:x :y| x + y] value:3 value:5.

[ProfStef next] value.'! !

!SmalltalkSyntaxTutorial methodsFor: 'lessons' stamp: 'pmon 5/20/2012 23:09'!
blocksAssignation
	^ Lesson
title: 'Block assignation' 
lesson: 
'"Blocks can be assigned to a variable then executed later.

Note that |b| is the declaration of a variable named ''b'' and that '':='' assigns a value to a variable.

Select the three lines then Print It:"

|b|
b := [:x | x+2].
b value: 12.

ProfStef next.'! !

!SmalltalkSyntaxTutorial methodsFor: 'lessons' stamp: 'pmon 5/20/2012 23:11'!
conditionals
	^ Lesson
title: 'Conditionals' 
lesson: 
'"Conditionals are just messages sent to Boolean objects"

1 < 2
  ifTrue: [100]
  ifFalse: [42].

"Here the message is ifTrue:ifFalse

Try this:"

TranscriptWindow openTranscript.

3 > 10 
	ifTrue: [Transcript show: ''maybe there''''s a bug ....'']
	ifFalse: [Transcript show: ''No : 3 is less than 10''].

3 = 3 ifTrue: [ProfStef next].'.! !

!SmalltalkSyntaxTutorial methodsFor: 'lessons' stamp: 'pmon 5/20/2012 23:55'!
debugger
	^ Lesson
title: 'Debugger' 
lesson: '"The Debugger may be the most famous tool of Smalltalk environments. It will open as soon as an unmanaged Exception occurs. 

The following code will open the debugger on the message stack, select SmalltalkSyntaxTutorial>>divideTwoByZero".

SmalltalkSyntaxTutorial new divideTwoByZero. '! !

!SmalltalkSyntaxTutorial methodsFor: 'interactive' stamp: 'pmon 5/26/2012 23:28'!
divideTwoByZero
	2/0.
	
	"Oups!! 2/0 raises a ZeroDivide exception.  So the debugger opens to let you fix the code.
	
	- Remove the line of code above.
	- Right-click and select 'Accept' to compile the new version of the method
	- click the button 'Proceed' to continue execution.
	".
	ProfStef next. ! !

!SmalltalkSyntaxTutorial methodsFor: 'lessons' stamp: 'pmon 5/20/2012 19:39'!
doingVSPrinting 
	^ Lesson
title: 'Doing VS Printing: Doing' 
lesson: 
'"Cool !! (I like to say Cooool :) ). You''ve just executed a Smalltalk expression. More precisely, you sent the message ''next'' to ProfStef class (it''s me !!).

Note you can run this tutorial again by evaluating: ''ProfStef go''. 
''ProfStef previous'' returns to the previous lesson.

You can also Do It using the keyboard shortcut ''ALT d''
(this varies according to your operating system/computer: it can be ''CMD d'' or ''CTRL d''). 

Try to evaluate these expressions:"

BrowserWindow openBrowser.

Smalltalk aboutThisSystem.

"Then go to the next lesson:"

ProfStef next.'! !

!SmalltalkSyntaxTutorial methodsFor: 'initialize-release' stamp: 'pmon 5/21/2012 00:00'!
initialize
	super initialize.
	self prepareDebuggerExample.! !

!SmalltalkSyntaxTutorial methodsFor: 'lessons' stamp: 'pmon 5/26/2012 23:20'!
instanciation
	^ Lesson
title: 'Instanciation' 
lesson: 
'"Objects are instances of their class. Usually, we send the message #new to a class for creating an instance of this class.

The message #allInstances sent to a class answers an Array with all instances of this class.

For example, let''s look at how many instances of EllipseMorph exist:"

EllipseMorph allInstances size.

"Now create a new instance of it:"

EllipseMorph new openInWorld.
	
"See the ellipse on the screen ? The list of all instances should contains one more instance:"

EllipseMorph allInstances size.

"Let''s play with it:"

EllipseMorph allInstances last 
	color: Color red.

"Let''s delete it and ask the system to clean the memory:"

EllipseMorph allInstances last delete.
Smalltalk garbageCollect.
EllipseMorph allInstances size.

ProfStef next'
! !

!SmalltalkSyntaxTutorial methodsFor: 'lessons' stamp: 'pmon 5/20/2012 23:16'!
iterators
	^ Lesson
title: 'Iterators' 
lesson: 
'"The message do: is sent to a collection of objects (Array, Set, OrderedCollection), evaluating the block for each element.

Here we want to print all the numbers on the Transcript (a console)"

#(11 38 3 -2 10) do: [:each |
	     Transcript show: each printString; newLine].

"Some other really nice iterators"

#(11 38 3 -2 10) collect: [:each | each abs].

#(11 38 3 -2 10) collect: [:each | each odd].

#(11 38 3 -2 10) select: [:each | each odd].

#(11 38 3 -2 10) select: [:each | each > 10].

#(11 38 3 -2 10) reject: [:each | each > 10].

#(11 38 3 -2 10) 
     do: [:each | Transcript show: each printString]
     separatedBy: [Transcript show: ''.''].

ProfStef allInstances do: [:aProfStef | aProfStef next].'! !

!SmalltalkSyntaxTutorial methodsFor: 'lessons' stamp: 'pmon 5/20/2012 23:14'!
loops
	^ Lesson
title: 'Loops' 
lesson: 
'"Loops are high-level collection iterators, implemented as regular methods."

"Basic loops:
  to:do:
  to:by:do"

1 to: 100 do:
  [:i | Transcript show: i asString; newLine ].

1 to: 100 by: 3 do: [:i | Transcript show: i asString; newLine].

100 to: 0 by: -2 do: 
    [:i | Transcript show: i asString; newLine].

1 to: 1 do: [:i | ProfStef next].'! !

!SmalltalkSyntaxTutorial methodsFor: 'lessons' stamp: 'pmon 5/20/2012 22:55'!
mathematicalPrecedence
	^ Lesson
title: 'Mathematical precedence'
lesson: 
'"Traditional precedence rules from mathematics do not follow in Smalltalk."

2 * 10 + 2.

"Here the message * is sent to 2, which answers 20, then 20 receive the message +

Remember that all messages always follow a simple left-to-right precedence rule, * without exceptions *."

2 + 2 * 10.

2 + (2 * 10).

8 - 5 / 2.

(8 - 5) / 2.

8 - (5 / 2).

ProfStef next.'! !

!SmalltalkSyntaxTutorial methodsFor: 'lessons' stamp: 'pmon 5/20/2012 22:39'!
messageSyntaxBinary
	^ Lesson
title: 'Message syntax: Binary messages' 
lesson: 
'"Binary messages have the following form:
    anObject + anotherObject"

3 * 2.

Date today + 3 weeks.

false | false.

true & true.

true & false.

10 @ 100.

10 <= 12.

''ab'', ''cd''.

Date today < Date yesterday.

ProfStef next.'! !

!SmalltalkSyntaxTutorial methodsFor: 'lessons' stamp: 'pmon 5/20/2012 23:02'!
messageSyntaxCascade
	^ Lesson
title: 'Message syntax: Cascade' 
lesson: 
'"; is the cascade operator. It''s useful to send message to the SAME receiver
Open a Transcript (console):"

TranscriptWindow openTranscript.

"Then:"

Transcript show: ''hello''.
Transcript show: ''Smalltalk''.
Transcript newLine.

"is equivalent to:"

Transcript 
	   show: ''hello'';
	   show: ''Smalltalk'' ;
	   newLine.

"Try to go to the next lesson with a cascade of two ''next'' messages:"

ProfStef'.! !

!SmalltalkSyntaxTutorial methodsFor: 'lessons' stamp: 'pmon 5/20/2012 23:04'!
messageSyntaxCascadeShouldNotBeHere
	^ Lesson
title: 'Lost ?' 
lesson: 
'"Hey, you should not be here !!!! 

Go back and use a cascade !!"

ProfStef previous.'.! !

!SmalltalkSyntaxTutorial methodsFor: 'lessons' stamp: 'pmon 5/20/2012 22:52'!
messageSyntaxExecutionOrder
	^ Lesson
title: 'Message syntax: Execution order' 
lesson: 
'"Unary messages are executed first, then binary messages and finally keyword messages:
    Unary > Binary > Keywords"

2 + 3 squared.

2 raisedTo: 3 + 2.

(0@0) class.

0@0 corner: 100@200.

(0@0 corner: 100@200) class.

"Between messages of similar precedence, expressions are executed from left to right"

-3 abs negated reciprocal.

ProfStef next.'! !

!SmalltalkSyntaxTutorial methodsFor: 'lessons' stamp: 'pmon 5/20/2012 22:54'!
messageSyntaxExecutionOrderParentheses
	^ Lesson
title: 'Message syntax: Parentheses'
lesson: 
'"Use parentheses to change order of evaluation"

(2 + 3) squared.

(2 raisedTo: 3) + 2.

(0@0 extent: 100@200) bottomRight.

ProfStef next.'! !

!SmalltalkSyntaxTutorial methodsFor: 'lessons' stamp: 'pmon 5/20/2012 22:47'!
messageSyntaxKeyword
	^ Lesson
title: 'Message syntax: Keyword messages' 
lesson: 
'"Keyword Messages are messages with arguments. They have the following form:
    anObject akey: anotherObject akey2: anotherObject2"

4 between: 0 and: 10.

"The message is between:and: sent to the Number 4"

1 max: 3.

Color r:1 g:0 b:0.

"The message is r:g:b: implemented on class Color. Note you can also write"

Color
	r:1
	g:1
	b:0.
		
ProfStef perform: #next.'! !

!SmalltalkSyntaxTutorial methodsFor: 'lessons' stamp: 'pmon 5/21/2012 00:03'!
messageSyntaxUnary
	^ Lesson
title: 'Message syntax: Unary messages' 
lesson: 
'"Messages are sent to objects. There are three types of message: Unary, Binary and Keyword.

Unary messages have the following form:
    anObject aMessage 

You''ve already sent unary messages. For example:"

1 class.

false not.

Time now.

Date today.

Float pi.

"And of course: "

ProfStef next.'! !

!SmalltalkSyntaxTutorial methodsFor: 'lessons' stamp: 'pmon 5/20/2012 23:54'!
pharoEnvironment
	^ Lesson
title: 'Cuis environment' 
lesson: 
'"Every Smalltalk system is full of objects. There are windows, text, numbers, dates, colors, points and much more. You can interact with objects in a much more direct way than is possible with other programming languages.

Every object understands the message ''explore''. As a result, you get an Explorer window that shows details about the object."

Date today explore.

"This shows that the date object consists of a point in time (start) and a duration (one day long)."

ProfStef explore.

"You see, ProfStef class has a lot of objects. Let''s take a look at my code:"

ProfStef browseClassHierarchy.

ProfStef next.'! !

!SmalltalkSyntaxTutorial methodsFor: 'interactive' stamp: 'pmon 5/21/2012 00:01'!
prepareDebuggerExample

	self class compile: 'divideTwoByZero
	2/0.
	
	"Oups!! 2/0 raises a ZeroDivide exception.  So the debugger opens to let you fix the code.
	
	- Remove the line of code above.
	- Right-click and select ''Accept'' to compile the new version of the method
	- click the button ''Proceed'' to continue execution.
	".
	ProfStef next. '
	
	classified: 'interactive'.! !

!SmalltalkSyntaxTutorial methodsFor: 'lessons' stamp: 'pmon 5/20/2012 19:31'!
printing
	^ Lesson
title: 'Doing VS Printing: Printing' 
lesson: 
'"Now you''re a Do It master !! Let''s talk about printing. It''s a Do It which prints the result next to the expression you''ve selected.
For example, select the text below, open the menu and click on ''print it (p)'':"

1 + 2.

"You''ve seen the letter ''p'' between parentheses next to ''print it'' ? It indicates the ALT- shortcut to execute this command.

Try ALT-p on the following expressions:"

Date today.

Time now.

"The result is selected, so you can erase it using the backspace key. Try it !!"

Smalltalk datedVersion.

ProfStef next.'! !

!SmalltalkSyntaxTutorial methodsFor: 'lessons' stamp: 'pmon 5/26/2012 23:24'!
reflection
	^ Lesson
title: 'Reflection' 
lesson: 
'"You can inspect and change the system at runtime.

Take a look at method #ifFalse:ifTrue: source code of class True:"

(True>>#ifFalse:ifTrue:) getSource.

' "Or just its comment: "

"(True>>#ifFalse:ifTrue:) comment." " <-- pmon: not able to find a way to retrieve the method comment "

, '"Here''s all the methods I implement:"

ProfStef selectors.


"Let''s create a new method to go to the next lesson:"

ProfStef class compile:''goToNextLesson
  self next''.

"Wow !! I can''t wait to use my new method !! "

ProfStef goToNextLesson.'! !

!SmalltalkSyntaxTutorial methodsFor: 'lessons' stamp: 'pmon 5/20/2012 23:49'!
reflectionContinued
	^ Lesson
title: 'Reflection continued' 
lesson: 
'"So cool, isn''t it ?  Before going further, let''s remove this method:"

ProfStef respondsTo: #goToNextLesson.

ProfStef class removeSelector: #goToNextLesson.

ProfStef respondsTo: #goToNextLesson.


"Then move forward:"

ProfStef default executeMethod: (ProfStef lookupSelector:#next).'! !

!SmalltalkSyntaxTutorial methodsFor: 'lessons' stamp: 'pmon 5/20/2012 23:58'!
theEnd
	^ Lesson
title: 'Tutorial done !!' 
lesson: 
'"This tutorial is done. Enjoy programming Smalltalk with Cuis. 

Don''t forget to read ''Pharo By Example'' found here: http://pharo-project.org/PharoByExample.

You can run this tutorial again by evaluating: ProfStef go.

Do you want to create your own interactive tutorial with ProfStef ? That''s very easy!!!!  How ? There''s a ProfStef interactive tutorial for that :D
Just evaluate the following code:

ProfStef goOn: HowToMakeYourOwnTutorial

See you soon !!"
'! !

!SmalltalkSyntaxTutorial methodsFor: 'tutorial' stamp: 'pmon 5/21/2012 00:00'!
tutorial
^ #(
welcome
doingVSPrinting
printing

basicTypesNumbers
basicTypesCharacters
basicTypesString
basicTypesSymbol
basicTypesArray
basicTypesDynamicArray

messageSyntaxUnary
messageSyntaxBinary
messageSyntaxKeyword
messageSyntaxExecutionOrder
messageSyntaxExecutionOrderParentheses
mathematicalPrecedence
messageSyntaxCascade
messageSyntaxCascadeShouldNotBeHere

blocks
blocksAssignation
conditionals
loops
iterators

instanciation

reflection
reflectionContinued

pharoEnvironment
debugger

theEnd
)! !

!SmalltalkSyntaxTutorial methodsFor: 'lessons' stamp: 'pmon 5/20/2012 18:14'!
welcome
	^ Lesson
title: 'Welcome' 
lesson: 
'"Hello!! I''m Professor Stef.

You must want me to help you learn Smalltalk.

So let''s go to the first lesson.  Select the text below, right-click and choose ''do it (d)''"

ProfStef next.'! !

!TutorialPlayer methodsFor: 'accessing' stamp: 'pmon 5/20/2012 16:12'!
currentLesson
	^ self tutorial lessonAt: self tutorialPosition.! !

!TutorialPlayer methodsFor: 'navigating' stamp: 'pm 5/21/2012 23:05'!
first
	self rewind.
	^ self currentLesson! !

!TutorialPlayer methodsFor: 'navigating' stamp: 'pm 5/21/2012 23:05'!
last
	tutorialPosition := self size.
	^ self currentLesson! !

!TutorialPlayer methodsFor: 'navigating' stamp: 'pm 5/21/2012 23:05'!
next
	self tutorialPosition < self size
		ifTrue: [tutorialPosition := tutorialPosition + 1].
	^ self currentLesson! !

!TutorialPlayer methodsFor: 'navigating' stamp: 'pm 5/21/2012 23:06'!
previous
	tutorialPosition >  1 ifTrue: [tutorialPosition := tutorialPosition  - 1].
	^ self currentLesson! !

!TutorialPlayer methodsFor: 'navigating' stamp: 'pm 5/21/2012 23:06'!
rewind
	tutorialPosition := 1.! !

!TutorialPlayer methodsFor: 'accessing' stamp: 'pmon 5/20/2012 16:12'!
size
	^ self tutorial size! !

!TutorialPlayer methodsFor: 'accessing' stamp: 'pmon 5/20/2012 16:12'!
tutorial
	^ tutorial  ifNil: [tutorial := SmalltalkSyntaxTutorial new]! !

!TutorialPlayer methodsFor: 'accessing' stamp: 'pmon 5/20/2012 16:12'!
tutorial: aTutorial
	tutorial := aTutorial! !

!TutorialPlayer methodsFor: 'accessing' stamp: 'pmon 5/20/2012 16:13'!
tutorialPosition 
	^ tutorialPosition  ifNil: [
		self rewind.
		tutorialPosition.
	].	! !

!TutorialPlayer methodsFor: 'accessing' stamp: 'pmon 5/20/2012 16:13'!
tutorialPosition: aTutorialPosition 
	tutorialPosition := aTutorialPosition 
	! !
