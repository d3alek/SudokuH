{-
Copyright (c) 2012, Aleksandar Kodzhabashev
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

module GUI where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
import Data.IORef
import Data.Char
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import SudokuSolver

sample1 :: [Int]
sample1 = [3, -1, 6, 8, -1, -1, -1, -1, -1, 1, -1, 9, -1, -1, 5, -1, -1, -1 ,-1,
		-1, -1, -1, 7, -1, -1, 2, -1, 4, -1, -1, 7, -1, -1, -1, -1, 1, 9, -1, 
		-1, -1, -1, -1, -1, -1, 7, 6, -1, -1, -1, -1, 8, -1, -1, 5, -1, 4, -1, 
		-1, 8, -1, -1, -1, -1, -1, -1, -1, 2, -1, -1, 1, -1, 6, -1, -1, -1, -1, 
		-1, 1, 8, -1, 3]

-- Function which returns the head of the string as a digit. Returns -1 if string is empty.
stringToDigit :: String -> Int
stringToDigit str 
	| str /= "" = ((\c -> ord c - ord '0').head) str
	| otherwise = -1

-- Code modified from the GTK insertText docs
insertTextAction :: (EditableClass self, WidgetClass self) => self -> IO ()
insertTextAction entry = do
	-- Make entry contents selected on click
	entry `on` buttonReleaseEvent $ do
		button <- eventButton	
		liftIO (editableSelectRegion entry 0 (-1))
		return False
	
	-- From the GTK documentation:
	-- To modify the text that the user inserts, you need to connect to this 
	-- signal, modify the text the way you want and then call editableInsertText. 
	-- To avoid that this signal handler is called recursively, you need to 
	-- temporarily block it using signalBlock. After the default signal handler 
	-- has inserted your modified text, it is important that you prevent the 
	-- default handler from being executed again when this signal handler returns. 
	-- To stop the current signal, use stopInsertText
	idRef <- newIORef undefined
 	id <- entry `on` insertText $ \str pos -> do
 		id <- readIORef idRef
 		signalBlock id
		-- Filter entry input to digits only
		newStr <- 
			if isDigit (last str) 
			then return [last str]
			else return []
 		pos' <- editableInsertText entry newStr pos
		
		-- Change focus to the next field if text is inserted
		if newStr /= []
		then (do
			parent <- ( liftM fromJust (widgetGetParent entry))
			parentsParent <- ( liftM fromJust (widgetGetParent parent))
			widgetChildFocus parentsParent DirTabForward
			return Nothing)
		else return Nothing
		signalUnblock id
 		stopInsertText id
 		return pos'
 	writeIORef idRef id


clearEntriesText :: EditableClass self => [self] -> IO ()
clearEntriesText = mapM_ (\entry -> editableDeleteText entry 0 1)	

setEntriesDigits :: EditableClass self => [Int] -> [self] -> IO ()
setEntriesDigits digits entries = mapM_ (\(value, entry) -> editableInsertText entry (show value) 0) (filter (\(d,e) -> d /= -1) (zip digits entries)) 

main = do
	initGUI

	builder <- builderNew
	builderAddFromFile builder "gui.glade"

	mainWindow <- builderGetObject builder castToWindow "main_window"
	onDestroy mainWindow mainQuit

	-- Name the 81 fields a1, a2, ..., b1, b2, ...., i8, i9	
	fieldNames <- return [lettr:num:[] | lettr <- ['a'..'i'], num <- ['1'..'9']]
	fieldEntries <- mapM (builderGetObject builder castToEntry) fieldNames

	-- Set the properties of every field according to insertTextAction
	mapM insertTextAction fieldEntries

	solveButton <- builderGetObject builder castToButton "btn_solve"
	onClicked solveButton $ do 
		fieldText <- mapM (\entry -> editableGetChars entry 0 1) fieldEntries
		fieldDigits <- return $ map stringToDigit fieldText
		solution <- return $ solveCSP $ zip fieldNames fieldDigits
		if solution /= [] 
		then do 
			clearEntriesText fieldEntries
			setEntriesDigits (snd (unzip solution)) fieldEntries
		else return ()

	clearButton <- builderGetObject builder castToButton "btn_clear"
	onClicked clearButton $ clearEntriesText fieldEntries

	quitButton <- builderGetObject builder castToButton "btn_quit"
	onClicked quitButton $ do widgetDestroy mainWindow --Deprecated	

	sampleButton <- builderGetObject builder castToButton "btn_sample"
	onClicked sampleButton $ do
		clearEntriesText fieldEntries
		setEntriesDigits sample1 fieldEntries

	--TODO: Set focus chain to be the lines of the big square, not the small squares
	--allCellsContainer <- builderGetObject builder castToTable "all_cells_table"
	--containerSetFocusChain allCellsContainer (map castToWidget fieldEntries)
	
	widgetShowAll mainWindow
	mainGUI

