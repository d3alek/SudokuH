SudokuH
=======

A Sudoku Solver using MAC+MRV algorithm written in Haskell

GUI is written using Gtk2Hs (http://projects.haskell.org/gtk2hs/)

Thanks to Petros Papapanagiotou for his CSP Framework.


Description
-----------

  * GUI.hs - The main program loop and GUI initialization.

	* SudokuSolver.hs - The heart of the program, packs the Sudoku solving algorithms. See the comments for description on what exactly is happening there.

	* gui.glade - An XML file with the design of the program, which is read by the program during execution. Created with Glade (http://glade.gnome.org/).

	* CSPframework.hs - A CSP framework definition written by Petros Papapanagiotou. The Sudoku solving algorithms use this framework.

Usage (Linux)
-------------

Run one of the precompiled binaries bin/sudokuh_32 and bin/sudokuh_64 on Linux 32bit and 64bit respectively (Tested on Arch Linux only, but should work on anything with GTK installed).

The binaries need the gui.glade file (they use it to build the interface).

Alternatively, see the Build Instructions to build it from source. 


Usage (Windows)
---------------

I could not find a way to build it on Windows7x64 using the latest version of gtk2hs and following the official instructions on http://code.haskell.org/gtk2hs/INSTALL. What I get are plenty of "undefined reference" linker errors. I shall try again soon, but until then - sorry, no Windows version. You can try compiling it yourself and if you succeed, do tell me how.


Build Instructions
------------------

To build it by yourself execute the following command in the SudokuH root folder:

ghc --make GUI.hs -o SudokuSolver -main-is GUI

You need to have ghc (Glasgow Haskell Compiler) and gtk2hs installed. For more information on how to get gtk2hs see http://projects.haskell.org/gtk2hs/download/
