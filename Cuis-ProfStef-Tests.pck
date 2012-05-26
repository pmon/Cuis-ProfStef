'From Cuis 4.0 of 21 April 2012 [latest update: #1291] on 26 May 2012 at 11:32:09 pm'!
'Description Please enter a description for this package '!
!classDefinition: #AbstractTutorialTest category: #'Cuis-ProfStef-Tests'!
TestCase subclass: #AbstractTutorialTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-ProfStef-Tests'!
!classDefinition: 'AbstractTutorialTest class' category: #'Cuis-ProfStef-Tests'!
AbstractTutorialTest class
	instanceVariableNames: ''!

!classDefinition: #HowToMakeYourOwnTutorialTest category: #'Cuis-ProfStef-Tests'!
TestCase subclass: #HowToMakeYourOwnTutorialTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-ProfStef-Tests'!
!classDefinition: 'HowToMakeYourOwnTutorialTest class' category: #'Cuis-ProfStef-Tests'!
HowToMakeYourOwnTutorialTest class
	instanceVariableNames: ''!

!classDefinition: #LessonTestInstanciation category: #'Cuis-ProfStef-Tests'!
TestCase subclass: #LessonTestInstanciation
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-ProfStef-Tests'!
!classDefinition: 'LessonTestInstanciation class' category: #'Cuis-ProfStef-Tests'!
LessonTestInstanciation class
	instanceVariableNames: ''!

!classDefinition: #MockLessonView category: #'Cuis-ProfStef-Tests'!
Object subclass: #MockLessonView
	instanceVariableNames: 'lesson title'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-ProfStef-Tests'!
!classDefinition: 'MockLessonView class' category: #'Cuis-ProfStef-Tests'!
MockLessonView class
	instanceVariableNames: ''!

!classDefinition: #MockTutorial category: #'Cuis-ProfStef-Tests'!
AbstractTutorial subclass: #MockTutorial
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-ProfStef-Tests'!
!classDefinition: 'MockTutorial class' category: #'Cuis-ProfStef-Tests'!
MockTutorial class
	instanceVariableNames: ''!

!classDefinition: #MockTutorial2 category: #'Cuis-ProfStef-Tests'!
MockTutorial subclass: #MockTutorial2
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-ProfStef-Tests'!
!classDefinition: 'MockTutorial2 class' category: #'Cuis-ProfStef-Tests'!
MockTutorial2 class
	instanceVariableNames: ''!

!classDefinition: #ProfStefTestGo category: #'Cuis-ProfStef-Tests'!
TestCase subclass: #ProfStefTestGo
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-ProfStef-Tests'!
!classDefinition: 'ProfStefTestGo class' category: #'Cuis-ProfStef-Tests'!
ProfStefTestGo class
	instanceVariableNames: ''!

!classDefinition: #ProfStefTestGoOnMockTutorial category: #'Cuis-ProfStef-Tests'!
TestCase subclass: #ProfStefTestGoOnMockTutorial
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-ProfStef-Tests'!
!classDefinition: 'ProfStefTestGoOnMockTutorial class' category: #'Cuis-ProfStef-Tests'!
ProfStefTestGoOnMockTutorial class
	instanceVariableNames: ''!

!classDefinition: #ProfStefTestNavigation category: #'Cuis-ProfStef-Tests'!
TestCase subclass: #ProfStefTestNavigation
	instanceVariableNames: 'prof mockView'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-ProfStef-Tests'!
!classDefinition: 'ProfStefTestNavigation class' category: #'Cuis-ProfStef-Tests'!
ProfStefTestNavigation class
	instanceVariableNames: ''!

!classDefinition: #SmalltalkSyntaxTutorialTest category: #'Cuis-ProfStef-Tests'!
TestCase subclass: #SmalltalkSyntaxTutorialTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-ProfStef-Tests'!
!classDefinition: 'SmalltalkSyntaxTutorialTest class' category: #'Cuis-ProfStef-Tests'!
SmalltalkSyntaxTutorialTest class
	instanceVariableNames: ''!

!classDefinition: #TutorialPlayerTestTutorialAccessor category: #'Cuis-ProfStef-Tests'!
TestCase subclass: #TutorialPlayerTestTutorialAccessor
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-ProfStef-Tests'!
!classDefinition: 'TutorialPlayerTestTutorialAccessor class' category: #'Cuis-ProfStef-Tests'!
TutorialPlayerTestTutorialAccessor class
	instanceVariableNames: ''!

!classDefinition: #TutorialPlayerTestWithMockTutorial category: #'Cuis-ProfStef-Tests'!
TestCase subclass: #TutorialPlayerTestWithMockTutorial
	instanceVariableNames: 'player'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-ProfStef-Tests'!
!classDefinition: 'TutorialPlayerTestWithMockTutorial class' category: #'Cuis-ProfStef-Tests'!
TutorialPlayerTestWithMockTutorial class
	instanceVariableNames: ''!


!AbstractTutorialTest methodsFor: 'tests' stamp: 'pmon 5/26/2012 19:12'!
testLessonsReturnsAllLessonInstances
	|lessons|
	lessons := MockTutorial new lessons.
	self assert: 3 equals: lessons size.
	self assert: 'first' equals: lessons first title.
	self assert: 'second' equals: (lessons at:2 ) title.
	self assert: 'third' equals: lessons last title.! !

!AbstractTutorialTest methodsFor: 'tests' stamp: 'pmon 5/26/2012 19:12'!
testTitleHumanizeClassName
	self assert: 'How To Make Your Own Tutorial' equals: HowToMakeYourOwnTutorial title.
	self assert: 'Smalltalk Syntax Tutorial' equals: SmalltalkSyntaxTutorial title.! !

!AbstractTutorialTest methodsFor: 'tests' stamp: 'pmon 5/26/2012 19:48'!
testTutorialRaisesShouldBeImplemented
	| tutorial |
	tutorial := AbstractTutorial new.
	self should: [ tutorial tutorial ] raise: Error .

"
	self 
		should: [tutorial tutorial]
		raise: Error
		withExceptionDo: [:anException | 
			self 
				assert:  ShouldBeImplemented 
				equals: anException class
		].
"! !

!AbstractTutorialTest methodsFor: 'tests' stamp: 'pmon 5/26/2012 19:53'!
testTutorialsReturnsAllButMockTutorial
	| tutorials |
	tutorials := AbstractTutorial tutorials.
	self assert: (tutorials includes: SmalltalkSyntaxTutorial).
	self assert: (tutorials includes: HowToMakeYourOwnTutorial).
	self deny: (tutorials includes: MockTutorial).! !

!HowToMakeYourOwnTutorialTest methodsFor: 'tests' stamp: 'pmon 5/26/2012 22:28'!
testEachSelectorExists
	self testedTutorial tutorial do: [:aSelector|
		self assert: (self testedTutorial respondsTo: aSelector) 
	]! !

!HowToMakeYourOwnTutorialTest methodsFor: 'tests' stamp: 'pmon 5/26/2012 20:39'!
testEachSelectorReturnsALesson
	| answer |
	self testedTutorial tutorial do: [:aSelector|
		answer := (self testedTutorial perform: aSelector).
		self assert: (answer isKindOf: Lesson).
	]! !

!HowToMakeYourOwnTutorialTest methodsFor: 'tests' stamp: 'pmon 5/26/2012 20:39'!
testLessonAtReturnsCorrespondingLesson
	| answer |
	1 to: (self testedTutorial tutorial size) do: [:index|
		answer := self testedTutorial lessonAt: index.
		self assert: (answer isKindOf: Lesson)
	]
	! !

!HowToMakeYourOwnTutorialTest methodsFor: 'tests' stamp: 'pmon 5/26/2012 20:40'!
testNotEmpty
	self assert: self testedTutorial tutorial notEmpty.! !

!HowToMakeYourOwnTutorialTest methodsFor: 'tests' stamp: 'pmon 5/26/2012 20:40'!
testSizeReturnsNumberOfSelectors
	self assert: (self testedTutorial tutorial size) equals: self testedTutorial size.! !

!HowToMakeYourOwnTutorialTest methodsFor: 'requirements' stamp: 'pmon 5/26/2012 20:31'!
testedTutorial
	"Returns an instance of an AbstractTutorial subclass"
	^ HowToMakeYourOwnTutorial new! !

!LessonTestInstanciation methodsFor: 'tests' stamp: 'pmon 5/26/2012 19:57'!
testNewLessonHaveEmptyStringForTitleLesson
	| newLesson  |
	newLesson := Lesson new.
	self assert: newLesson title equals: ''.
	self assert: newLesson lesson equals: ''.! !

!LessonTestInstanciation methodsFor: 'tests' stamp: 'pmon 5/26/2012 19:57'!
testTitleLessonCreation
	| firstLesson secondLesson |
	firstLesson := Lesson title: 'First lesson' lesson: 'Smalltalk rules!!'.
	secondLesson := Lesson title: 'Second lesson' lesson: 'ProfStef is cool'.
	
	self assert: firstLesson title equals: 'First lesson'.
	self assert: firstLesson lesson equals: 'Smalltalk rules!!'.
	
	self assert: secondLesson title equals: 'Second lesson'.
	self assert: secondLesson lesson equals: 'ProfStef is cool'.! !

!MockLessonView methodsFor: 'accessing' stamp: 'pmon 5/26/2012 19:58'!
lesson
	^ lesson! !

!MockLessonView methodsFor: 'gui' stamp: 'LaurentLaffont 1/21/2010 16:10'!
showLesson: aLesson withTitle: aString	lesson := aLesson.	title := aString.! !

!MockLessonView methodsFor: 'gui' stamp: 'DannyChan 2/1/2010 22:02'!
showTutorialNode: aTutorialNode	lesson:= aTutorialNode lessonInstance.	title := aTutorialNode title.! !

!MockLessonView methodsFor: 'accessing' stamp: 'pmon 5/26/2012 19:58'!
title
	^ title! !

!MockTutorial methodsFor: 'lesson' stamp: 'pmon 5/26/2012 19:54'!
firstLesson
	^ Lesson title: 'first' lesson: 'First lesson'.! !

!MockTutorial methodsFor: 'lesson' stamp: 'pmon 5/26/2012 19:54'!
secondLesson
	^ Lesson title: 'second' lesson: 'Second lesson'.! !

!MockTutorial methodsFor: 'lesson' stamp: 'pmon 5/26/2012 19:55'!
thirdLesson
	^ Lesson title: 'third' lesson: 'Third lesson'.! !

!MockTutorial methodsFor: 'tutorial' stamp: 'LaurentLaffont 1/21/2010 15:14'!
tutorial 	^ #(firstLessonsecondLessonthirdLesson)! !

!ProfStefTestGo methodsFor: 'running' stamp: 'pmon 5/26/2012 19:59'!
tearDown
	ProfStef default close! !

!ProfStefTestGo methodsFor: 'tests' stamp: 'pmon 5/26/2012 19:59'!
testGoOnMockTutorial
	| displayedText expected |
	ProfStef goOn: MockTutorial.
	
	displayedText := ProfStef default lessonView text.
	expected := MockTutorial new firstLesson lesson.
 	self assert: displayedText equals: expected.! !

!ProfStefTestGo methodsFor: 'tests' stamp: 'pmon 5/26/2012 19:59'!
testGoOpenSmalltalkSyntaxTutorial
	| displayedText expected |
	ProfStef go.
	
	displayedText := ProfStef default lessonView text.
	expected := SmalltalkSyntaxTutorial new welcome lesson.
 	self assert: displayedText equals: expected.! !

!ProfStefTestGo methodsFor: 'tests' stamp: 'pmon 5/26/2012 20:18'!
testGoTwiceShowLessonViewIfNotVisible
	| firstLessonView |
	ProfStef go.
	firstLessonView := ProfStef default lessonView.
	firstLessonView close.
	ProfStef goOn: SmalltalkSyntaxTutorial.
	self assert: ( (SystemWindow allInstances ) includes: firstLessonView window ).

"
	self assert:  (World systemWindows includes:  firstLessonView window).
"! !

!ProfStefTestGo methodsFor: 'tests' stamp: 'pmon 5/26/2012 20:05'!
testGoTwiceUseSameLessonView
	| firstLessonView |
	ProfStef go.
	firstLessonView := ProfStef default lessonView.
	ProfStef goOn: SmalltalkSyntaxTutorial.
	self assert: (firstLessonView ==  ProfStef default lessonView).! !

!ProfStefTestGoOnMockTutorial methodsFor: 'running' stamp: 'pmon 5/26/2012 20:21'!
setUp
	ProfStef tutorial: MockTutorial lesson: #firstLesson ! !

!ProfStefTestGoOnMockTutorial methodsFor: 'running' stamp: 'pmon 5/26/2012 20:21'!
tearDown
	ProfStef default close! !

!ProfStefTestGoOnMockTutorial methodsFor: 'tests' stamp: 'pmon 5/26/2012 20:20'!
testFirstLessonShouldBeDisplayed.
 	self assert:  'First lesson' equals: ProfStef default lessonView text! !

!ProfStefTestGoOnMockTutorial methodsFor: 'tests' stamp: 'pmon 5/26/2012 20:20'!
testLastShouldGoToThirdLesson
	ProfStef last.
 	self assert:  'Third lesson' equals: ProfStef default lessonView text! !

!ProfStefTestGoOnMockTutorial methodsFor: 'tests' stamp: 'pmon 5/26/2012 20:20'!
testLastThenFirstShouldGoToFirstLesson
	ProfStef last; first.
 	self assert:  'First lesson' equals: ProfStef default lessonView text! !

!ProfStefTestGoOnMockTutorial methodsFor: 'tests' stamp: 'pmon 5/26/2012 20:21'!
testLastThenPreviousShouldGoToSecondLesson
	ProfStef last; previous.
 	self assert:  'Second lesson' equals: ProfStef default lessonView text! !

!ProfStefTestGoOnMockTutorial methodsFor: 'tests' stamp: 'pmon 5/26/2012 20:21'!
testNextShouldGoToSecondLesson
	ProfStef next.
 	self assert:  'Second lesson' equals: ProfStef default lessonView text! !

!ProfStefTestNavigation methodsFor: 'running' stamp: 'pmon 5/26/2012 20:22'!
setUp
	prof := ProfStef new.
	prof player: (
		TutorialPlayer new 
			tutorial: MockTutorial new; 
			yourself).
	mockView := MockLessonView new.
	prof lessonView: mockView.
	prof open.! !

!ProfStefTestNavigation methodsFor: 'tests' stamp: 'pmon 5/26/2012 20:21'!
testNextOpenSecondLesson
	prof next.
	self assert: mockView title equals: 'second (2/3)'.
	self assert: mockView lesson lesson equals: 'Second lesson'.! !

!ProfStefTestNavigation methodsFor: 'tests' stamp: 'pmon 5/26/2012 20:21'!
testSequenceNextNextOpenThirdLesson
	prof next; next.
	self assert: mockView title equals: 'third (3/3)'.
	self assert: mockView lesson lesson equals: 'Third lesson'.! !

!ProfStefTestNavigation methodsFor: 'tests' stamp: 'pmon 5/26/2012 20:21'!
testSequenceNextNextPreviousOpenSecondLesson
	prof next; next; previous.
	self assert: mockView title equals: 'second (2/3)'.
	self assert: mockView lesson lesson equals: 'Second lesson'.! !

!ProfStefTestNavigation methodsFor: 'tests' stamp: 'pmon 5/26/2012 20:22'!
testShowFirstLessonOnGo
	| lesson |
	lesson := mockView lesson.
	self assert: lesson title equals: 'first'.
	self assert: lesson lesson equals: 'First lesson'.! !

!ProfStefTestNavigation methodsFor: 'as yet unclassified' stamp: 'pmon 5/26/2012 20:22'!
testShowingLessonByIndex
	prof tutorial: MockTutorial lessonAt: 2.
	self assert: mockView title equals: 'second (2/3)'.
	self assert: mockView lesson lesson equals: 'Second lesson'.! !

!ProfStefTestNavigation methodsFor: 'as yet unclassified' stamp: 'pmon 5/26/2012 20:22'!
testShowingLessonBySelector
	prof tutorial: MockTutorial lesson: #firstLesson.
	self assert: mockView title equals: 'first (1/3)'.
	self assert: mockView lesson lesson equals: 'First lesson'.! !

!SmalltalkSyntaxTutorialTest methodsFor: 'tests' stamp: 'pmon 5/26/2012 20:27'!
testDivideTwoByZeroSignalsZeroDivide
	[self testedTutorial divideTwoByZero.
	self fail] 
	on: ZeroDivide 
	do: []! !

!SmalltalkSyntaxTutorialTest methodsFor: 'tests' stamp: 'pmon 5/26/2012 22:31'!
testEachSelectorExists
	self testedTutorial tutorial do: [:aSelector|
		self assert: (self testedTutorial respondsTo: aSelector) 
	]! !

!SmalltalkSyntaxTutorialTest methodsFor: 'tests' stamp: 'pmon 5/26/2012 20:39'!
testEachSelectorReturnsALesson
	| answer |
	self testedTutorial tutorial do: [:aSelector|
		answer := (self testedTutorial perform: aSelector).
		self assert: (answer isKindOf: Lesson).
	]! !

!SmalltalkSyntaxTutorialTest methodsFor: 'tests' stamp: 'pmon 5/26/2012 20:39'!
testLessonAtReturnsCorrespondingLesson
	| answer |
	1 to: (self testedTutorial tutorial size) do: [:index|
		answer := self testedTutorial lessonAt: index.
		self assert: (answer isKindOf: Lesson)
	]
	! !

!SmalltalkSyntaxTutorialTest methodsFor: 'tests' stamp: 'pmon 5/26/2012 20:39'!
testNotEmpty
	self assert: self testedTutorial tutorial notEmpty.! !

!SmalltalkSyntaxTutorialTest methodsFor: 'tests' stamp: 'pmon 5/26/2012 20:40'!
testSizeReturnsNumberOfSelectors
	self assert: (self testedTutorial tutorial size) equals: self testedTutorial size.! !

!SmalltalkSyntaxTutorialTest methodsFor: 'requirements' stamp: 'pmon 5/26/2012 20:28'!
testedTutorial
	"Returns an instance of an AbstractTutorial subclass"
	^ SmalltalkSyntaxTutorial new! !

!TutorialPlayerTestTutorialAccessor methodsFor: 'tests' stamp: 'pmon 5/26/2012 20:23'!
testDefaultsToSmalltalkSyntaxTutorial
	| player |
	player := TutorialPlayer new.
	self assert: (player tutorial isKindOf: SmalltalkSyntaxTutorial).! !

!TutorialPlayerTestTutorialAccessor methodsFor: 'tests' stamp: 'pmon 5/26/2012 20:23'!
testWithMockTutorial
	| mockTutorial player |
	mockTutorial := MockTutorial new.
	player := TutorialPlayer new tutorial: mockTutorial; yourself.
	self assert: player tutorial equals: mockTutorial.! !

!TutorialPlayerTestWithMockTutorial methodsFor: 'running' stamp: 'pmon 5/26/2012 20:23'!
setUp
	| tutorial |
	tutorial := MockTutorial new.
	player := TutorialPlayer new tutorial: (MockTutorial new)! !

!TutorialPlayerTestWithMockTutorial methodsFor: 'tests' stamp: 'pmon 5/26/2012 20:24'!
testCurrentLessonIsFirstOneAtCreation
	self assert: player currentLesson title equals: 'first'.! !

!TutorialPlayerTestWithMockTutorial methodsFor: 'tests' stamp: 'pmon 5/26/2012 20:24'!
testNavigation
	self assert: player next title equals: 'second'.
	self assert: player currentLesson title equals: 'second'.
	
	self assert: player next title equals: 'third'.
	self assert: player currentLesson title equals: 'third'.
	
	self assert: player next title equals: 'third'.
	self assert: player currentLesson title equals: 'third'.
	
	self assert: player previous title equals: 'second'.
	self assert: player currentLesson title equals: 'second'.
	
	self assert: player previous title equals: 'first'.
	self assert: player currentLesson title equals: 'first'.
	
	self assert: player previous title equals: 'first'.
	self assert: player currentLesson title equals: 'first'.! !

!TutorialPlayerTestWithMockTutorial methodsFor: 'tests' stamp: 'pmon 5/26/2012 20:24'!
testResetTutorialGoBackToFirstLesson
	player next; next.
	self assert: player currentLesson title equals: 'third'.
	
	player rewind.
	self assert: player currentLesson title equals: 'first'.! !

!TutorialPlayerTestWithMockTutorial methodsFor: 'tests' stamp: 'pmon 5/26/2012 20:24'!
testSizeReturnsThree
	self assert: player size equals: 3.! !
