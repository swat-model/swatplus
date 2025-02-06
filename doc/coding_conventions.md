# Fortran 90/95 SWAT+ Coding Conventions

This document contains coding conventions recommended for SWAT+ development in FORTRAN 90.

The purpose of this document is to ensure that new FORTRAN code will be as portable
and robust as possible, as well as consistent throughout the model. It builds
upon commonly shared experience to avoid error-prone practices and gathers
guidelines that are known to make codes more robust.

This document covers topic in order of decreasing importance (see below), deemed
to be important for any code. It is recognized in the spirit of this standard
that certain suggestions which make code easier to read for some people (e.g.
lining up attributes, or using all lower case or mixed case) are subjective and
therefore should not have the same weight as techniques and practices that are
known to improve code quality. For this reason, the standards within this document
are divided into three components: Requirements, Recommendations, and Encouraged practices:

> __Required__
> : Aimed at ensuring portability, readability, and robustness. Compliance with this category is mandatory.

> __Recommended__
> : Good practices. Compliance with this category is strongly recommended. The case for deviations will need to be argued by the programmer.

> __Encouraged__
> : Compliance with this category is optional, but is encouraged for consistency purposes.

Modeller may opt to adhere to all three levels or just the first two. All development must adhere at least to the mandatory standards.

Below are some references for more background information on this topic.

* http://dbwww.essc.psu.edu/lasdoc/programmer/4fortran.html
* http://box.mmm.ucar.edu/wrf/WG2/WRF_conventions.html
* http://water.usgs.gov/software/code/general/sysdoc/doc/coding.pdf
* http://www.personal.psu.edu/faculty/h/d/hdk/fortran.html
* http://xmm.vilspa.esa.es/sas/current/doc/devel/coding.html
* http://projects.osd.noaa.gov/spsrb/standards_docs/fortran95_standard_rev26sep2007.pdf

<!--
## General Coding Guidelines
* Reduce or eliminate global variable usage.
* Attempt to limit the number of arguments in argument list - long lists make it hard to reuse.
* Limit of only one return point per subroutine or function.
* Use exceptions as error indicators if supported.
* Components should be specific to one and only one purpose.
* Components with side effects are not allowed (e.g. don't mix I/O code  with computational code).
* Program against a standard (e.g., ANSI C, C++, Java, FORTRAN 77/90/95) -
* Make sure your code compiles under different compilers and platforms.
* Use preprocessor directives for adaptation to different architectures/compilers/OS.
* Make I/O specific components separate from computatonal components.
* Avoid static allocation of data (compile time allocation).
* Be most specific with your data types.
* Avoid using custom data types for argument types.-->

## Interoperability and Portability

### Required

* Source code must conform to the ISO Fortran 95 standard.
* No compiler- or platform-dependent extensions shall be used.
* No use shall be made of compiler-dependent error specifier values (e.g. `IOSTAT` or `STAT` values).
* Source code must compile successfully using ``gfortran`` that is part of the GNU Compiler Collection, even though the Intel compiler is later used for creating the executable.
* Note that ``EXIT(N)`` is a F90/95 standard. ``STOP`` should be avoided to terminate the model. It is recognized
  that STOP does not necessarily return an error code. If an error code must be passed to a script for
  instance, then ``EXIT`` must be used but within a central place, so that to limit its
  occurrences within the code to a single place.

### Recommended

* Precision: Parametrizations should not rely on vendor-supplied flags to supply a default floating
  point precision or integer size. The F90/95 ``KIND`` feature should be used instead.
* Do not use tab characters in the code to ensure it will look as intended when ported. They are not
  part of the FORTRAN characters set.

### Encouraged

* For applications requiring interaction with independently-developed frameworks, the use of ``KIND`` type
  for all variables declaration is encouraged to facilitate the integration.

## Readability

### Required

* Use free format syntax.
* Use consistent indentation across the code. Each level of indentation should use at least two spaces.
* Use modules to organize source code.
* FORTRAN keywords (e.g., ``DATA``) shall not be used as variable names.
* Use meaningful, understandable names for variables and parameters. Recognized abbreviations are acceptable as a means
  of preventing variable names getting too long.
* Each externally-called function, subroutine, should contain a header. The content and style of the header should be
  consistent across the system, and should include the functionality of the function, as well as the description of the
  arguments, the author(s) names. A header could be replaced by a limited number of descriptive comments for small
  subroutines.
* Magic numbers should be avoided; physical constants (e.g., pi, gas constants) should never be hard-coded
  into the executable portion of a code; use ``PARAMETER`` statements instead.
* Hard-coded numbers should be avoided when passed through argument lists since a compiler flag,
  which defines a default precision for constants, cannot be guaranteed.
* Do not use ``GOTO`` statements. These are hard to maintain and complicate understanding
  the code. If absolutely necessary to use GOTO (if using other constructs complicates
  the code structure), thoroughly document the use of the ``GOTO``.

### Recommended

* Use construct names to name loops, to increase readability, especially in nested loops.
* Similarly, use construct names in subroutines, functions, main programs, modules, operator, interface, etc.
* Include comments to describe the input, output and local variables of all procedures. Grouping comments for
  similar variables is acceptable when their names are explicit enough.
* Use comments as required to delineate significant functional sections of code.
* Do not use FORTRAN statements and intrinsic function names as symbolic names.
* Use named parameters instead of “magic numbers”; ``REAL, PARAMETER :: PI=3.14159, ONE=1.0``

### Encouraged

* When writing new code, adhere to the style standards within your own coding style. When
  modifying an old code, adhere to the style of the existing code to keep consistency.
* Use the same indentation for comments as for the rest of the code.
* Functions, procedures, data that are naturally linked should be grouped in modules.
* Limit the number of characters per line, maximum of 132
* Use of operators ``<, >, <=, >=, ==, /=`` is encouraged (for readability) instead
  of ``.lt., .gt., .le., .ge., .eq., .ne.``
* Modules should be named the same name as the files they reside in: To simplify
  the makefiles that compile them. Consequently, multiple modules in a single file are to be avoided where possible.
* Use blanks to improve the appearance of the code, to separate syntactic elements
  (on either side of equal signs, etc) in type declaration statements
* Always use the ``::`` notation, even if there are no attributes.
* Line up vertically: attributes, variables, comments within the variables declaration section.
* Remove unused variables
* Remove code that was used for debugging once this is complete.

## Robustness

### Required

* Use ``IMPLICIT NONE`` in all source code: main programs, modules, etc. to ensure correct
  size and type declarations of variables/arrays.
* Use ``PRIVATE`` in modules before explicitly listing data, functions, procedures
  to be ``PUBLIC``. This ensures encapsulation of modules and avoids potential naming
  conflicts. Exception to previous statement is when a module is entirely dedicated
  to ``PUBLIC`` data/functions (e.g. a module dedicated to constants).
* Initialize all variables. Do not assume machine default value assignments.
* Do not initialize variables of one type with values of another.

### Recommended

* Do not use the operators ``==`` and ``/=`` with floating-point expressions as operands.
  Check instead the departure of the difference from a pre-defined numerical accuracy threshold (e.g. epsilon comparison).
* In mixed mode expressions and assignments (where variables of different types
  are mixed), the type conversions should be written explicitly (not assumed). Do
  not compare expressions of different types for instance. Explicitly perform the type conversion first.
* No include files should be used. Use modules instead, with ``USE`` statements in
  calling programs.
* Structures (derived types) should be defined within their own module. Procedures,
  Functions to manipulate these structures should also be defined within this module, to form an object-like entity.
* Procedures should be logically flat (should focus on a particular functionality, not several ones)
* Module ``PUBLIC`` variables (global variables) should be used with care and mostly
  for static or infrequently varying data.

### Encouraged

* Use parentheses at all times to control evaluation order in expressions.
* Use of structures is encouraged for a more stable interface and a more compact design.
  Refer to structure contents with the % sign (e.g. ``Absorbents%WaterVapor``).

## Arrays

### Required

* Subscript expressions should be of type integer only.
* When arrays are passed as arguments, code should not assume any particular passing mechanism.

### Recommended

* Use of arrays is encouraged as well as intrinsic functions to manipulate them.
* Use of assumed shapes is fine in passing vectors/arrays to functions/arrays.

### Encouraged

* Declare ``DIMENSION`` for all non-scalars

## Dynamic Memory Allocation / Pointers

### Required

* Use of allocatable arrays is preferred to using pointers, when possible. To
  minimize risks of memory leaks and heap fragmentation.
* Use of pointers is allowed when declaring an array in a subroutine and making
  it available to a calling program.
* Always initialize pointer variables in their declaration statement using the
  ``NULL()`` intrinsic. ``INTEGER, POINTER :: x=> NULL()``
* The preferable mechanism for dynamic memory allocation is automatic arrays, as
  opposed to ``ALLOCATABLE`` or ``POINTER`` arrays for which memory must be explicitly
  allocated and deallocated; space allocated using ``ALLOCATABLE`` or ``POINTER`` must
  be explicitly freed using the ``DEALLOCATE`` statement.

### Recommended

* Always deallocate allocated pointers and arrays. This is especially important
  inside subroutines and inside loops.
* Always test the success of a dynamic memory allocation and deallocation - the
  ``ALLOCATE`` and ``DEALLOCATE`` statements have an optional argument to allow this.
* In a given program unit do not repeatedly ``ALLOCATE`` space, ``DEALLOCATE`` it and
  then ``ALLOCATE`` a larger block of space - this will almost certainly generate large
  amounts of unusable memory.

### Encouraged

* Use of dynamic memory allocation is encouraged. It makes code generic and avoids
  declaring with maximum dimensions.
* For simplicity, use Automatic arrays in subroutines whenever possible, instead
  of allocatable arrays.

## Looping

### Required

* Do not use ``GOTO`` to exit/cycle loops, use instead ``EXIT`` or ``CYCLE`` statements.

### Recommended

* No numbered ``DO`` loops such as  ``(DO 10 ...10 CONTINUE)``.

## Functions/Procedures

### Required

* The ``SAVE`` statement is discouraged; use module variables for state saving.
* Do not use an entry in a function subprogram.
* Functions must not have pointer results.
* The names of intrinsic functions (e.g., ``SUM``) shall not be used for user-defined functions.
* Procedures that return a single value should be functions; note that single values could
  also be user-defined types.
* All communication with the module should be through the argument list or
  it should access its module variables.

### Recommended

* All dummy arguments, except pointers, should include the INTENT clause in their declaration
* Limit use of type specific intrinsic functions (e.g., ``AMAX, DMAX - use MAX`` in all cases).
* Avoid statically dimensioned array arguments in a function/subroutine.
* Check for invalid argument values.

### Encouraged

* Error conditions. When an error condition occurs inside a function/procedure, a
  message describing what went wrong should be printed. The name of the routine in
  which the error occurred must be included. It is acceptable to terminate execution
  within a package, but the developer may instead wish to return an error flag through
  the argument list.
* Functions/procedures that perform the same function but for different types/sizes
  of arguments, should be overloaded, to minimize duplication and ease the maintainability.
* When explicit interfaces are needed, use modules, or contain the subroutines in the
  calling programs (through ``CONTAINS`` statement), for simplicity.
* Do not use external routines as these need interface blocks that would need to
  be updated each time the interface of the external routine is changed.

## I/O

### Required

* I/O statements on external files should contain the status specifier parameters ``err=, end=, iostat=,``
  as appropriate.
* All global variables, if present, should be set at the initialization stage.

### Recommended

* Avoid using ``NAMELIST`` I/O if possible.
* Use write rather than print statements for non-terminal I/O.
* Use Character parameters or explicit format specifiers inside the Read or
  Write statement. DO not use labeled format statements (outdated).

## Fortran Features that are obsolescent and/or discouraged:

### Required

* No Common blocks. Modules are a better way to declare/store static data, with the added
  ability to mix data of various types, and to limit access to contained variables
  through use of the ONLY and PRIVATE clauses.
* No assigned and computed GO TOs - use the ``CASE`` construct instead
* No arithmetic ``IF`` statements - use the block IF construct instead
* Use ``REAL`` instead of ``DOUBLE PRECISION``
* Avoid ``DATA, ASSIGN Labeled DO BACKSPACE Blank COMMON, BLOCK DATA ``
* Branch to ``END IF`` outside the block ``IF``
* DO non-integer Control
* Hollerith Constants
* ``PAUSE``
* multiple ``RETURN``
* Alternate RETURN

### Recommended

* Do not make use of the equivalence statement, especially for variables of different types.
  Use pointers or derived types instead.

### Encouraged

* No implicitly changing the shape of an array when passing it into a subroutine.
  Although actually forbidden in the standard it was very common practice in FORTRAN 77
  to pass 'n' dimensional arrays into a subroutine where they would, say, be treated as
  a 1 dimensional array. This practice, though banned in Fortran 90, is still possible
  possiblewith external routines for which no Interface block has been supplied.
  This only works because of assumptions made about how the data is stored.

## Source Files

### Required

* Document the function interface: argument name, type, unit, description, constraint,defaults.
* The ``INCLUDE`` statement shall not be used; use the ``USE`` statement instead.
* Try to limit source column length, including comments, to 80 columns (or follow language specific limits).

### Recommended

* A component should not exceed 300-500 effective lines of code, be efficient with your coding.
* Use blank lines (or lines with a standard character in column 1) to separate
  statement blocks to improve code readability.
* Apply consistent indentation method for code.
* Module/subprogram names shall be lower case; the name of a file containing a
  module/subprogram shall be the module/subprogram name with the suffix ``*.f90``."

### Encouraged

* Clearly separate declaration of argument variables from declaration of  local variables.
* Use descriptive and unique names for variables and subprograms (so as to improve the code readability and facilitate global string search);
* try to limit name lengths to 12-15 characters.
* Indent continuation lines to ensure that, for example, parts of a multi-line equation line up in a readable manner.
* Start comment text with a standard character (e.g. !, C, etc.); if a stand-alone
  line then start comment character in the first column.
