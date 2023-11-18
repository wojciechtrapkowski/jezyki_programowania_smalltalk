Object subclass: #Polygon
	instanceVariableNames: 'vertices name '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'JezykiProgramowania'!

    !Polygon methodsFor: 'initialize-release'!
    
    initialize: amountOfVertices name: newName
        "Constructor of an object - polygon"
    
    	name := newName.
    	vertices := Array new: amountOfVertices.
    	vertices at: 1 put: 0@0!
    
    !Polygon methodsFor: 'accessing'!
    
    " Getter for name variable "
    name
    	^name!
    
    " Getter for vertices variable "	
    vertices
        ^vertices!
    	
    " Setter for name variable"
    name: newName
    	name:=newName! 
    	
    getSideLength
        ^((vertices at: 1) x - (vertices at: 2) x) abs !

    getHeight
        ^0 !
    	
    print
        Transcript
            show: name, ': '; cr.
        vertices do: [ :each |
            Transcript
                show: '    Point: (', (each x asString printString), ', ', (each y asString printString), ')'; nl.
        ].
        Transcript
            show: '    Surface area: ', self surfaceArea printString; cr. ! !


    !Polygon methodsFor: 'actions'!
    
    surfaceArea
        ^0!!


Polygon subclass: #Square
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'JezykiProgramowania'!

!Square methodsFor: 'arithmetic'!

" Add surface area of two squares and then create new object with the side length equal to squared root of calculated surface area. "
+ other
    " Define local variable "
	| calculatedField |

	calculatedField := self surfaceArea + other surfaceArea.
	^(Square new) initialize: calculatedField sqrt! !

!Square methodsFor: 'actions'!

transform
    | halfOfSideLength translatedVertices |
    halfOfSideLength := self getSideLength / 2 asFloat. 
    translatedVertices := vertices collect: [ :each |
        each - halfOfSideLength.
    ].
    vertices := translatedVertices !
    
    
" Calculate field of a square - method "
surfaceArea
	^self getSideLength squared! !

!Square methodsFor: 'initialize-release'!

" Create a square with given side length "
initialize: sideLength
	super initialize: 4 name: 'Square'.
	vertices at: 2 put: sideLength@0.
	vertices at: 3 put: sideLength@sideLength.
	vertices at: 4 put: 0@sideLength.! !


"---------------------------------------------------"


Polygon subclass: #EquilateralTriangle 
    instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'JezykiProgramowania'!
    
    !EquilateralTriangle methodsFor: 'accessing'!
        getHeight
            ^(self getSideLength * (3 sqrt)) / 2 asFloat! !
    
    !EquilateralTriangle methodsFor: 'actions'!
    
    transform
        | halfOfSideLength thirdOfHeight tempVertices newTriangle|
        halfOfSideLength := (self getSideLength) / 2 asFloat. 
        thirdOfHeight := (self getHeight) / 3 asFloat.
        tempVertices := (self vertices) deepCopy.
        
        Transcript
            show: 'Centering Equilateral Triangle with center at: ', halfOfSideLength printString, '@', thirdOfHeight printString; cr. 
            
        1 to: tempVertices size do: [:i |
            (tempVertices at: i) x: ((tempVertices at: i) x) - halfOfSideLength.
            (tempVertices at: i) y: ((tempVertices at: i) y) - thirdOfHeight.
        ].
        
        " Creating new object with tempVertices " 
        
        newTriangle := EquilateralTriangle new initialize: (self getSideLength).
        
        1 to: tempVertices size do: [:i |
            newTriangle vertices at: i put: (tempVertices at: i).
        ].
        
        newTriangle print. 
        
        ^newTriangle. 
        !
    
    surfaceArea
	    ^((self getSideLength squared) * ( 3 sqrt )) / 4! !
    
    !EquilateralTriangle methodsFor: 'arithmetic'!

    + other
    	| calculatedField |
    
    	calculatedField := self surfaceArea + other surfaceArea.
    	^(EquilateralTriangle new) initialize: ((calculatedField*4) / ( 3 sqrt )) sqrt! !

    !EquilateralTriangle methodsFor: 'initialize-release'!
    
    initialize: sideLength
        super initialize: 3 name: 'Equilateral Triangle'.
        vertices at: 2 put: sideLength@0.
        vertices at: 3 put: (sideLength / 2 asFloat)@(self getHeight). ! !


" Given tests "
k := (Square new) initialize:2.
t := (EquilateralTriangle new) initialize: 2.
Transcript show: 'Given are polygons:'; cr.
t print.
k print.
Transcript cr; show: 't+k'; nl.
t2 := t + k.
t2 print.
Transcript cr; show: 'k+t'; nl.
k1 := k + t.
k1 print.

k3 := k transform.
k3 print.
t4 := t transform.
t4 print.

" Additional tests "

Transcript nl; show: 'Additional tests: '; nl.

| mySquare mySquare2 mySquare3 |
mySquare := Square new initialize: 15.
mySquare2 := Square new initialize: 120. 

mySquare3 := mySquare + mySquare2.
mySquare3 print.

mySquare3 := mySquare transform.

| myT myT2 myT3 |
myT := EquilateralTriangle new initialize: 5.
myT2 := EquilateralTriangle new initialize: 8.

myT3 := myT transform.
myT3 := myT2 transform.

myT3 := myT + mySquare.
myT3 print.

myT3 := myT2 + mySquare2.
myT3 print.

myT3 := myT2 + mySquare2.
myT3 print.