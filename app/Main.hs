{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.GI.Base
import Control.Monad 
import Data.Text (Text, pack)
import Data.GI.Base.GType (gtypeString)
import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango

demoList::[(Text, Text)]

demoList = [("1","James"),("2","John"),("3","Robert"),("4","Michael"),("5","William"),("6","David"),("7","Richard"),("8","Joseph"),("9","Thomas"),("10","Charles")]
 
setValuesToListStore :: Gtk.ListStore ->  [(Text, Text)] -> Int -> IO Gtk.TreeIter

setValuesToListStore lsts artx counter = do

             let (stT::Text, stT2::Text) = artx !! counter

             gv <- toGValue (Just stT)
             gv2 <- toGValue (Just stT2)

             if (counter == (length artx)-1) then Gtk.listStoreInsertWithValuesv lsts (-1) [0, 1] [gv, gv2]              
             else do               
               m <- (Gtk.listStoreInsertWithValuesv lsts (-1) [0, 1] [gv, gv2])
               setValuesToListStore lsts artx (counter + 1)

main :: IO ()
main = do

    void $ Gtk.init Nothing

    window <- new Gtk.Window []
    on window #destroy Gtk.mainQuit

    column <- new Gtk.TreeViewColumn [ #title := "Column name" ]
    render <- new Gtk.CellRendererText [ #ellipsize :=  Pango.EllipsizeModeEnd
                                       , #editable := False ]
    #packStart column render True
    #addAttribute column render "text" 0

    column2 <- new Gtk.TreeViewColumn [ #title := "Column name2" ]
    render <- new Gtk.CellRendererText [ #ellipsize :=  Pango.EllipsizeModeEnd
                                       , #editable := False ]

    #packStart column2 render True
    #addAttribute column2 render "text" 1

    mlistStore <- Gtk.listStoreNew [gtypeString, gtypeString]
 
    setValuesToListStore mlistStore demoList 0

    view <- new Gtk.TreeView[#enableTreeLines := True, #headersVisible := True] 

    lk <- (Gtk.treeViewSetModel view (Just mlistStore))
 
    #appendColumn view column
    #appendColumn view column2

    #expandAll view

    #add window view

    #showAll window

    Gtk.main
