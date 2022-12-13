# COMP1100/1130 Assignment 1, Semester 1 2022

In this assignment, you will build a Haskell program that uses the
[CodeWorld API](https://hackage.haskell.org/package/codeworld-api-0.6.0/docs/CodeWorld.html) to draw colourful shapes on the screen.

{:.msg-info}
This assignment is worth **10%** of your final grade.

{:.msg-warn}
**Deadlines**: 
Part A: Thursday March 24, 2022, at 11:00pm Canberra time *sharp*
Part B: Sunday April 10, 2022, at 11:00pm Canberra time *sharp*
Please note that Daylight Savings ends on Sunday 3 April.

{:.msg-info}
Indicative marks and feedback for Part A will be returned in week 6.

---

## Required Knowledge

If you have finished the [Week 3 lab]({% link _labs/03.md %}), you should be able to complete Part A.

If you have finished the [Week 4 lab]({% link _labs/04.md %}), you should be able to complete the majority of the assignment. Some parts require recursion over
lists, which is covered in the [Week 5 lab]({%link _labs/05.md %}).

---

## Overview of Tasks


<table>
  <thead>
    <tr>
      <th> </th>
      <th><strong>COMP1100 Marks</strong></th>
      <th><strong>COMP1130 Marks</strong></th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>Task 1: Helper Functions</td>
      <td>20 marks</td>
      <td>10 marks</td>
    </tr>
    <tr>
      <td>Task 2: Rendering Shapes</td>
      <td>35 Marks</td>
      <td>30 Marks</td>
    </tr>
    <tr>
      <td>Task 3: Handling Events</td>
      <td>30 Marks</td>
      <td>25 Marks</td>
    </tr>
    <tr>
      <td>1130 Extensions</td>
      <td>-</td>
      <td>30 Marks</td>
    </tr>
    <tr>
      <td>Technical Report</td>
      <td>15 marks</td>
      <td>25 marks</td>
    </tr>
    <tr>
      <td>Total</td>
      <td>100 marks</td>
      <td>120 marks</td>
    </tr>
  </tbody>
</table>

**Part A** of the assignment requires you to complete Task 1.
**Part B** of the assignment requires you to complete all assigned tasks. As
you complete each task (or even each function within a task),
once your code compiles without errors, you
should commit and push your work with a sensible commit message.


{:.msg-info}
The purpose of Part A is to give you an opportunity to collect
feedback on your code and your progress in the course, and for us to
give you an indicative mark for your work so far. This will be
returned to you before the census date. Part A will be re-marked
alongside your Part B submission, giving you a final mark for the
assignment.

---

## Getting Started
1. Fork the assignment repository and create a project for it in
   VSCodium, following the same steps as in [Lab
   2]({% link _labs/02.md %}. The
   assignment repository is at
   <https://gitlab.cecs.anu.edu.au/comp1100/2022s1/2022s1studentfiles/Assignment1>.

2. Add **our** version of the repository as a *remote* called
   `upstream`. This allows us to provide additional fixes in the unlikely case that they
   are required. You do this by doing the following:
   - Go to the command palette in VSCode (or VSCodium) by pressing `Ctrl + Shift + p`
   - Type `git remote`
   - Click **Git: Add Remote**
   - Enter `upstream` into the box for the remote name
   - Put the following URL as the remote url: `https://gitlab.cecs.anu.edu.au/comp1100/2022s1/2022s1studentfiles/Assignment1.git`.

---

## Overview of the Repository

Most of your code will be written to Haskell files in the `src/`
directory. We are using the
[model-view-controller](https://en.wikipedia.org/wiki/Model–view–controller)
pattern to structure this assignment. Each file is called a _module_,
and we use modules to group related code together and separate
unrelated code.

### Model.hs

The _model_ is a data type that describes the state of the running
program. The program will move to new states (new values of type
`Model`) in response to user actions, as defined by the
_controller_.

### View.hs

The _view_ turns the _model_ into something that can be shown on the
screen; in this project, that is the CodeWorld `Picture` type.

### Controller.hs

The _controller_ considers user input (and other events), along with
the current _model_, and uses that to decide what the new _model_
should be.

### Other Files

* `tests/ShapesTest.hs` contains some _unit tests_ - simple checks
  that verify small parts of your program are working correctly. You
  are not required to write tests for this assignment, but you might
  find it useful to do so.

* `tests/Testing.hs` is a small testing library used by
  `tests/ShapesTest.hs`. You are not required to understand it for
  this assignment.

* `app/Main.hs` ties your functions together into the final program
  that runs. You are not required to understand it.

* `comp1100-assignment1.cabal` tells the cabal build tool how to build
  your assignment. You are not required to understand this file, and
  we will discuss how to use cabal below.

* `Setup.hs` tells cabal that this is a normal package with no unusual
  build steps. Some complex packages (that we won't see in this
  course) need to put more complex code here. You are not required to
  understand it.


## Overview of Cabal

`cabal` is the build tool for Haskell programs and libraries. It
provides several useful commands:

* `cabal v2-build`: Compile your assignment. Note that because of some code
  provided for you by us you will see some warnings about unused variables; you will
  fix these warnings during Task B, so may ignore them for Task A.

* `cabal v2-run shapes`: Build your assignment (if necessary), and run
  the `shapes` program. Note that you will need to enter `Ctrl-C` in your terminal
  to exit the program.

* `cabal v2-repl comp1100-assignment1`: Run the GHCi interpreter over
  your project. This gives you the same ghci environment you use in
  labs, but with the assignment code loaded. (Aside: REPL is the name
  for interactive sessions like GHCi - it stands for read-eval-print
  loop. Many modern languages have REPLs.)

* `cabal v2-test`: Build and run the tests. Tests will abort on the first
  failure, or the first call to a function that is still `undefined`.

{:.msg-info}
You should execute these cabal commands in the **top-level directory** of your
project, e.g. `~/comp1100/assignments/Assignment1` (i.e., the directory you are in when you
launch the VSCodium Terminal for your project).


## Overview of the Program

You use a web browser to interact with the `shapes` program that you
launched with `cabal v2-run shapes`. Once you have completed the
assignment, it will respond to the following actions:

| Action                     | Effect                                                                            |
|----------------------------|-------------------------------------------------------------|
| `Esc` (key)                | Clear the canvas                                            |
| `S` (key)                  | Display a sample image                                      |
| `C` (key)                  | Change colour (of shape to draw)                            |
| `T` (key)                  | Change tool (type of shape to draw)                         |
| `Backspace`/`Delete` (key) | Remove the last added shape                                 |
| `Spacebar` (key)           | When drawing a polygon, finish drawing the polygon, adding it to the canvas. Otherwise, nothing.                                                      |
| `+`/`=` (key)              | Increase the scaling factor for the rectangle by `0.1`      |
| `-`/`_` (key)              | Decrease the scaling factor for the rectangle by `0.1`. This number should not go below `0.1`                                                           |
| `D` (key)                  | Print the current `Model` to the terminal (useful for testing)                                                                                     |
| Click-drag-release (mouse) | Used to draw various shapes.                            |
| Click (mouse)              | Used to draw various shapes.                             | 


---

## Task 1: Helper Functions (COMP1100: 20 marks, COMP1130: 10 marks)

The easiest way to solve a large problem is often to break it apart
into smaller, easier problems. Programming is the same. In this task
you will write some helper functions that will make future tasks
easier. You can test your implementations by running `cabal v2-test`.

The functions you need to write for this task are:

* `toolToLabel` in `src/View.hs`. This function should return
  instructions for the user on how to use each `Tool`, according to
  the following table:

| Tool               | Label                                                                   |
|--------------------|-------------------------------------------------------------------------|
| `LineTool`         | `"Line: click-drag-release"`                                            |
| `PolygonTool`      | `"Polygon: click 3 or more times then spacebar"`                        |
| `CircleTool`       | `"Circle: click-drag-release between centre and circumference"`         |
| `TriangleTool`     | `"Triangle: click-drag release for first 2 corners"`                    |
| `RectangleTool`    | `"Rectangle: +/- to increase/decrease scaling factor; click-drag release for first 2 corners"` |
| `CapTool`          | `"Cap: click-drag-release for circle, then click for cap level"`         |


**Note:** At the time this assignment is released, the course will have only
briefly covered lists. You do not need to manipulate lists to write
`toolToLabel`; you can use a blank pattern (`_`) to ignore them.

* `nextColour` in `src/Controller.hs`. This function should return the
  next colour in our set of `ColourName`s:

| Argument | Result   |
|----------|----------|
| `Black`  | `Red`    |
| `Red`    | `Orange` |
| `Orange` | `Yellow` |
| `Yellow` | `Green`  |
| `Green`  | `Blue`   |
| `Blue`   | `Purple` |
| `Purple` | `White`  |
| `White`  | `Black`  |

* `nextTool` in `src/Controller.hs`. This function implements
  tool-switching, but should not change `Tool` if the user is halfway
  through an operation:

  - If the tool is not holding a point (that is, a non-`PolygonTool`
    tool holding `Nothing` (in all applicable fields) or a `PolygonTool` holding the empty list
    `[]`), select the next tool in the following sequence: `Line` ->
    `Polygon` -> `Circle`  -> `Triangle` -> `Rectangle`-> `Cap` -> `Line`.
    Note that the `Double` argument of `Rectangle` should be initialised at `1.0`.

  - If there is a `Point` stored in the given tool (because it is
    holding a `Just` value or the list in `PolygonTool` is non-empty),
    return the argument unchanged.

  - If this is unclear, study the `nextToolTests` in
    `test/ShapesTest.hs`.

**Note:** At the time this assignment is released, the course will have
only briefly covered lists. You can write the `PolygonTool` case for
`nextTool` without using list recursion. Use `[]` to match an empty
list. In a subsequent case, give the entire list a name like `points`
to match any nonempty list (or find a way to use the `_` pattern!).

---

**Part A ends here.**

---

### Submitting Part A

Your submission for Part A should include implementations of
`toolToLabel`, `nextColour`, and `nextTool` that
compile without warnings and pass the tests run by `cabal v2-test`.
You are welcome to continue working on Part B of your assignment
and committing and pushing changes, so long as your code continues 
to compile without errors and the tests continue to pass.

---

**Part B begins...**

---

## Task 2: Rendering Shapes (COMP1100: 35 marks, COMP1130: 30 marks)

In `src/View.hs`, `modelToPicture` converts your `Model` type into a
CodeWorld `Picture`, so that it can be displayed on the screen. It
currently does not work, because `colourShapesToPicture` is
`undefined`. In this task you will fill in that missing piece,
building up a function to convert the `[ColourShape]` from your
`Model` into a `Picture`. You can test these functions individually by
using `cabal v2-repl comp1100-assignment1`, using `drawingOf` to show
small pictures on the screen. You can also test everything as a whole
by launching the program with `cabal v2-run shapes` and pressing the `S`
key to show the sample image. The functions you need to write for
this task are all in `src/View.hs`:

* `colourNameToColour`: This function turns your `ColourName` type
  from the _model_ into a CodeWorld `Colour`. You should check [the
  CodeWorld
  documentation](https://hackage.haskell.org/package/codeworld-api-0.6.0/docs/CodeWorld.html#g:3) for information on colours.

* `shapeToPicture`: This function turns your `Shape` type into a
  CodeWorld `Picture`. You will need to consider the constructors for
  `Shape` individually, and work out the best way to turn each one
  into a `Picture`. Here are some hints to help you along:

  - CodeWorld has no function to draw a single line segment. It does
    have a function to draw a line made of multiple segments -
    `polyline`. It also has no function _for_ triangles, caps, or the rectangles
    of this assignment, but it does have functions that can draw these shapes.

  - Polygons, circles, triangles, rectangles, and caps should be drawn as
    solid (filled) `Picture`s.

  - Many of CodeWorld's functions draw individual shapes centred on
    the origin - `(0, 0)`. You will need to figure out how to slide
    (translate) the generated `Picture` so it shows up where it is
    supposed to go. Drawing diagrams will help. The `abs` function
    might also help - it computes the absolute value of its argument
    (i.e., `abs x == x` if `x > 0`, and `abs x == - x` otherwise).

  - (Isosceles) triangles should be defined by dragging from the apex to one
    of the vertices on the base. The base should run parallel to the x axis.
    Note that this means that triangles will look
    different depending on whether the user drags upwards or downwards. 

  - Rectangles are also defined by the user defining a first edge.
    The second vertex (where the pointer was released) will then be the start of the
    second edge, stretching out *clockwise* from the user-defined edge, and
    with length equal to the scaling factor of the rectangle multiplied by the length of
    the defined edge. Note that because of the clockwise requirement, the direction of
    the user's drag will matter, and that the rectangle will not necessarily run parallel
    to the x and y axes.

  - Caps are *circular segments* (not to be confused with sectors) defined by defining
    a circle by click-drag-release the same way as above, then clicking a point which determines
    the y coordinate below which the circle must be cut off (the x coordinate of this 
    click is not used). If the cutoff point is below the circle,
    the whole circle should remain intact, or "full". If the cutoff point is above,
    the whole circle should be clipped, and so the resultant (degenerate) segment will be "empty".
    Note that this task may require you to look through
    the CodeWorld documentation.

* `colourShapeToPicture`: This function should render the `Shape` and
  colour it using the Colour that corresponds to the given
  `ColourName`.

* `colourShapesToPicture`: This function should turn every
  `ColourShape` in a list into a single `Picture`. You will need
  to recurse over the input list. If you have not yet completed Lab 5,
  you may want to work on other parts of the assignment and come back
  to this.

* Here is the sample image for you to test your work against:
![sample image](Sample.png){:.w300px}

---

## Task 3: Handling Events (COMP1100: 30 marks, COMP1130: 25 marks)

It is now time to tackle `handleEvent` in
`src/Controller.hs`. CodeWorld calls this function whenever something
interesting happens (like a key press, a pointer press, or a
pointer release). This function is called with two arguments:

* The `Event` that just happened, and
* The current `Model` at the time the `Event` happened.

`handleEvent` then returns a new `Model` for the program to use moving
forward.

(Aside: [Elm](https://elm-lang.org) is a functional programming
language that uses a similar pattern to build front-end web
applications that are compiled to JavaScript.)

Let's trace a simple interaction. If the user wants to draw a *red
line* by clicking on the screen at coordinates $$(1, 1)$$ and
releasing the mouse at coordinates $$(2, 2)$$. starting at a blank
canvas, the state would transition as follows, starting with the
initial model:

1. `Model [] (LineTool Nothing) Black`

2. The user presses "C" to change the colour from black to red:

   `Model [] (LineTool Nothing) Red`

4. The user presses the mouse button at $$(1, 1)$$ changing the state to

   `Model [] (LineTool (Just (1.0,1.0))) Red`

5. The user releases the mouse button at $$(2, 2)$$ changing the state to

   `Model [(Line (1.0,1.0) (2.0,2.0),Red)] (LineTool Nothing) Red`

{:.msg-info}
Note that the `Tool` and the `ColourName` do not reset to the default values
after a shape has been drawn. However, the `Maybe Point` inside the tool
should revert to `Nothing`.


### Task 3.1: Handling Mouse Input

CodeWorld provides a few different event constructors for mouse input,
but the ones we're interested in here are `PointerPress` for when the user
clicks, and `PointerRelease` for when the user releases the mouse
button.

When a `PointerPress` event arrives, you will need to store it in the
current `Tool`. For everything except `PolygonTool`, you will store it
in the `Maybe Point` argument. For `PolygonTool`, you will add it to
the list of vertices. For `CapTool` you will have to consider whether
the press is intended to draw the defining circle, or give the y coordinate
to cut the circle off at.

When a `PointerRelease` event arrives, we can ignore it for
`PolygonTool`, as we will be finishing polygons using
the spacebar in Task 3.2. For almost everything else, a `PointerRelease` will
mean the end of a click-drag-release action, so you should construct the appropriate
shape and add it to the `[Shape]` in the `Model`. You should also
remove the starting point from the current `Tool`, so that future
shapes draw properly too. For `CapTool`, you will have to consider whether the
user is finishing defining the circle, or releasing after clicking for the y coordinate
cut-off.

Once you have finished this task for normal input, you may also want to consider
how your program will behave on unexpected input. For example, what should your program
do if it receives two consecutive `PointerPress` inputs without a `PointerRelease` between them? 

### Task 3.2: Handling Key Presses

To handle keyboard input, CodeWorld provides a `KeyPress` event. This
is already present in the assignment skeleton, because we have
implemented some keyboard functionality already. In the "Overview of
the Program" section, we listed the full set of keyboard commands that
your program will respond to. You need to implement the missing
functionality for these keys:

| Key                  | Effect                                                           |
|----------------------|------------------------------------------------------------------|
| `C`                  | Change colour (of shape to draw)                                 |
| `T`                  | Change tool (type of shape to draw)                              |
| `Backspace`/`Delete` | Remove the last added shape                                      |
| `Spacebar`           | Finish drawing a polygon, adding it to the canvas.               |
| `+`/`=`              | Increase the scaling factor for the rectangle by `0.1`           |
| `-`/`_` (key)        | Decrease the scaling factor for the rectangle by `0.1`. This number should not go below `0.1`                                                          |

If you have made it this far, you should not need to write a lot of
code to implement these. A few hints:

* Think back to Task 1.
* `Backspace`/`Delete` with no shapes drawn should not crash the program.
* Nor should any other unexpected input. Try to test some ``unexpected'' cases.
## Task 4: 1130 Extensions (COMP1130 Only: 30 Marks)
COMP1100 students are welcome to attempt extensions so long as that does not inhibit 
the function of tasks 1-4, but will not gain marks for completing them.
 
COMP1130 students will need to complete *two* of the *five* extensions from the pool below. 
If you attempt more than two, you must indicate which two you wish to be marked. Otherwise, 
we will mark the the first extensions you have completed in the order they appear in this 
document. 
If you change the behaviour of tool or colour switching, you may want to adjust the tests in 
`tests/ShapesTest.hs' so they continue to pass. They *must* continue to compile without errors.
*Note:* Extension functionality must be executed through the codeworld API just like 
the rest of the assignment. Interactions should be received through the `handleEvent` 
function, which you will need to modify. If you need to add argments to the model, please 
do so after the existing ones: `data Model = Model [ColourShape] Tool ColourName YourArgument1 YourArgument2 ...`
### Task 4.1: Save Points
Allow the user to save a drawing by clicking a key, so that they can return to that
picture (removing any changes made since) by clicking another key. The user should be
able to make any number of saves, and return to each of them in turn. For example, if they
save once, then a second time, there should be some means to return to the 
picture they had at the time of the first save.
### Task 4.2: Preview
Preview shapes as the user draws them. If the user has started to draw a shape, then you
should show the shape currently being drawn using a hollow (not solid) shape. When drawing
a cap, you should able to see the defining circle on the initial drag-and-release followed
by the circle cut at the current position of the pointer (i.e., how the cap would look 
were you to click at the current location of the mouse). 
### Task 4.3: Colour Picker
Add a tool allowing users to change the colour in use by clicking on part of the picture
with that colour. For example, if they click inside a red circle the colour in use
should change to (or remain at) red. There should be some sensible default behaviour if the
user clicks on a part of the picture that is not drawn on.
You may ignore non-convex polygons for this task; adding this functionality will not
attract further marks, but is an interesting challenge!
### Task 4.4: Regular polygons
Add a tool that draws regular polygons with any number of sides (equilateral triangles,
squares, etc...). The user should be able to modify how many sides will be drawn, and
draw with a single click-drag-and-release from the centre of the polygon to one of its vertices.
### Task 4.5: Animation
Your model need not only react to user inputs; it can also react to time passing, which
allows you to animate your pictures! You may choose which way(s) the shapes and/or
colours in your pictures change over time, although the animation must be dynamic
or depend on the picture in the model - a pre-defined repeating animation would not be sufficient.
Users should be able to turn animations on and off. Please be mindful of the speed at 
which your pictures change, so that you do not create an unpleasantly fast strobe-like effect.
---
## Technical Report (COMP1100: 15 marks, COMP1130 25 marks)
You should write a concise [technical
report]({% link _resources/08-reports.md %}) explaining your design choices in
implementing your program. The **maximum word count is 1000** for
COMP1100 students and **1500** for COMP1130 students.
This is a *limit*, not a *quota*; concise presentation
is a virtue.
{:.msg-warn}
Once again: These are not required word counts. They are the **maximum
number of words** that your marker will read. If you can do it in
fewer words without compromising the presentation, please do so.
Your report must be in PDF format, located at the root of your
assignment repository on GitLab and named `Report.pdf`. Otherwise, it
may not be marked, or will be marked but with a penalty. You should
double-check **on GitLab** that this is typed correctly.
The report must have a **title page** with the following items:
* Your name
* Your laboratory time and tutor
* Your university ID
### Content and Structure
Your audience is the tutors and lecturers, who are proficient at programming
and understand most concepts. Therefore you should not, for example, waste
words describing the syntax of Haskell or how recursion works. After reading
your technical report, the reader should thoroughly understand what problem
your program is trying to solve, the reasons behind major design choices in it,
as well as how it was tested. Your report should give a broad overview of your
program, but focus on the specifics of what *you* did and why.
Remember that the tutors have access to the above assignment
specification, and if your report *only* contains details from it then
you will only receive minimal marks. Below is an potential outline for
the structure of your report and some things you might discuss in it.
#### Introduction
If you wish to do so you can write an introduction. In it, give:
* A brief overview of your program:
  - how it works; and
  - what it is designed to do.
* If you have changed the way the controls work, perhaps for an extension,
  or added something that may make your program behave unexpectedly,
  then it would be worth making a note of it here.
This section is particularly relevant to more complicated programs.
#### Content
Talk about why you structured the program the way you did. Below are some
questions you could answer:
* Program design
  - Describe what each relevant function does conceptually. (i.e. how
    does it get you closer to solving the problems outlined in this
    assignment spec?)
  - How do these functions piece together to make the finished
    program? Why did you design and implement it this way?
  - What major design choices did *you* make regarding the functions
    that *you’ve* written and the overall structure of your program?
* Assumptions
  - Describe assumptions you have made about how a user might use the
    program and how this has influenced your design decisions.
* Testing
  - How did you test individual functions?
    - Be specific about this - the tutors know that you have tested
      your program, but they want to know *how*.
    - Describe the tests that prove individual functions on their own
      behave as expected (i.e. testing a function with different
      inputs and doing a calculation by hand to check that the outputs
      are correct).
  - How did you test the entire program? What tests did you perform to
    show that the program behaves as expected in all (even unusual)
    cases?
    - Again, be specific - did you check that you can draw shapes from
      left to right? What about right to left, or in different vertical
      directions? 
    - Have you checked edge cases (this is a computer science term
      that refers to unexpected or unlikely inputs that may cause a
      program to crash or behave in strange ways)? It is not likely
      that someone would try to change the tool halfway through
      drawing a shape, but it is essential that it has behaviour
      defined for that scenario. Describe similar tests that you have
      done to ensure the program can handle all inputs.
* Inspiration / external content
  - What resources did you use when writing your program (e.g.,
    published algorithms)?
  - If you have used resources such as a webpage describing an
    algorithm, be sure to cite it properly at the end of your report
    in a ‘References’ section. References do not count to the maximum
    word limit.
#### Reflection
Discuss the reasoning behind your decisions, rather than *what* the
decisions were. You can reflect on not only the decisions you made,
but the process through which you developed the final program:
* Did you encounter any conceptual or technical issues?
  - If you solved them, describe the relevant details of what happened
    and how you overcame them.
  - Sometimes limitations on time or technical skills can limit how
    much of the assignment can be completed. If you ran into a problem
    that you could not solve, then your report is the perfect place to
    describe them. Try to include details such as:
    - theories as to what caused the problem;
    - suggestions of things that might have fixed it; and
    - discussion about what you did try, and the results of these attempts.
    
  This is a great way to gain marks to make up for a not completely successful
  programming experience!
* What would you have done differently if you were to do it again
  - What changes to the design and structure you would make if you
    wrote the program again from scratch?
* Are parts of the program confusing for the reader? You can explain
  them in the report (in this situation you should also make use of
  comments in your code).
* If you collaborated with others, what was the nature of the
  collaboration?  (Note that you are only allowed to collaborate by
  discussing concepts, not sharing solutions.)
  - Collaborating is any discussion or work done together on planning
    or writing your assignment.
* Other info
  - You may like to briefly discuss details of events which were
    relevant to your process of design - strange or interesting things
    that you noticed and fixed along the way.
{:.msg-info}
This is a list of **suggestions**, not requirements. You should only
discuss items from this list if you have something interesting to
write.
### Things to avoid in a technical report
* Line by line explanations of large portions of code. (If you want to
  include a specific line of code, be sure to format as described in
  the "Format" section below.)
* Pictures of code or your IDE.
* Content that is not your own, unless cited.
* Grammatical errors or misspellings. Proof-read it before submission.
* Informal language - a technical report is a professional document, and as
  such should avoid things such as:
  - Unnecessary abbreviations (atm, wrt, ps, and so on), emojis, and
    emoticons; and
  - Stories / recounts of events not relevant to the development of the program.
* Irrelevant diagrams, graphs, and charts. Unnecessary elements will
  distract from the important content. Keep it succinct and focused.
If you need additional help with report writing, [ANU Academic Skills](https://www.anu.edu.au/students/academic-skills/appointments)
have resources to help.
### Format
You are not required to follow any specific style guide (such as APA
or Harvard). However, here are some tips which will make your report
more pleasant to read, and make more sense to someone with a computer
science background.
* Colours should be kept minimal. If you need to use colour, make sure it is
  absolutely necessary.
* If you are using graphics, make sure they are *vector* graphics (that stay
  sharp even as the reader zooms in on them).
* Any code, including type/function/module names or file names, that
  appears in your document should have a monospaced font (such as
  Consolas, Courier New, Lucida Console, or Monaco)
* Other text should be set in serif fonts (popular choices are Times,
  Palatino, Sabon, Minion, or Caslon).
* When available, automatic *ligatures* should be activated.
* Do not use underscore to highlight your text.
* Text should be at least 1.5 spaced.
---
## Communicating
**Do not** post your code publicly, either on Piazza or via other
forums. Posts on Piazza trigger emails to all students, so if by
mistake you post your code publicly, others will have access to your
code and you may be held responsible for plagiarism.
Once again, and we cannot stress this enough: **do not post your code
publicly** . If you need help with your code, post it *privately* to the
instructors.
When brainstorming with your friends, **do not share code**. There
might be pressure from your friends, but this is for both your and
their benefit. Anything that smells of plagiarism will be investigated
and there may be serious consequences.
Sharing concepts and sketches is perfectly fine, but sharing should stop
before you risk handing in suspiciously similar solutions.
Course staff will not look at assignment code unless it is posted
**privately** in piazza, or shared in a drop-in consultation.
Course staff will typically give assistance by asking questions,
directing you to relevant exercises from the labs, or definitions and
examples from the lectures.
{:.msg-info}
Before the assignment is due, course staff will not give individual
tips on writing functions for the assignment or how your code can be
improved. We will help you get unstuck by asking questions and
pointing you to relevant lecture and lab material. You will receive
feedback on you work when marks are released.
## Submission Checklist
Once you have finished your assignment, and preferably 24 hours prior
to the deadline, you should make sure that:
* You have fully read and understand the entire assignment
  specification.
* Your work has been pushed to GitLab.
* Your program compiles and runs, including the `cabal v2-test` test suite.
* Your program works on the lab machines - if the program does not
  work on the lab machines, it might fail tests used by the
  instructors. You can use the [VDI](https://comp.anu.edu.au/courses/comp1100/resources/02-working-remotely/) for these tests.
* You have proof-read and spell-checked your report.
* The report is in PDF format, located at the root of your project on
  GitLab and named `Report.pdf`. That capital `R` is important - Linux
  uses a case-sensitive file system. Otherwise, it may not be marked.
  Check this **on Gitlab** as the full filename may not always appear in your document.
