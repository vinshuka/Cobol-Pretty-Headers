# Cobol Pretty Headers

A VS Code extension to generate code required for report headers in Cobol

To create a header, first open up your cobol file and decide the location for the header data fields and the write procedure, best practice is to put a new line before and after the lines you want the data fields and the write procedure to start on. Then with the command pallet (ctrl-shift-p) use the command 'Create Header', this will open a new untitled file tab for you to design your header, this will also open the 'Header Title' view in the explorer that can be used to add or edit data field titles and the write procedure title. Type out your header the exact way you want it to appear in your report within the untitled tab. Once it looks the way you want it use the command pallet again to generate the cobol header code with the command 'Generate Header Code', a preview of the generated code will appear in the console output under 'Header Preview' which will appear automatically, note that if preview does not show you may have to swap terminal views to refresh the output console, once again when you are satisfied with the generated cobol, simply switch to your cobol file and place your cursor where you want your data fields and use the command 'Place Data Fields' from the command pallet, then do the same for the write procedure by using the command 'Place Write Procedure'. The inserted code will contain indentation to the correct cobol area, if you wish to do your own indentation simply disable indentation from the extension's settings.

## User Defined Settings

Highest and Lowest Level numbers can be set by the user, with the restriction that lowest cannot be a higher level than highest. Default values are 01 for highest and 05 for lowest.

The print output for the write procedure can also be set by the user, it should be the same value that you are writing your report data to. Default value is 'output'.

The user can also select whether or not they would like condensed headers or not.

Header:

Hello World  
My Name is  
Cobol!

Regular header:  
01 Hello-HDR.  
	05 FILLER	PIC X(5)	VALUE "Hello"  
	05 FILLER	PIC X(1)	VALUE SPACES  
	05 FILLER	PIC X(5)	VALUE "World"  
  
01 My-HDR.  
	05 FILLER	PIC X(2)	VALUE "My"  
	05 FILLER	PIC X(1)	VALUE SPACES  
	05 FILLER	PIC X(4)	VALUE "Name"  
	05 FILLER	PIC X(1)	VALUE SPACES  
	05 FILLER	PIC X(2)	VALUE "is"  
  
01 Cobol!-HDR.  
	05 FILLER	PIC X(6)	VALUE "Cobol!"  
  
  
write-hdrs.  
	WRITE 'PRINT-REC' FROM Hello-HDR  
	 	AFTER ADVANCING PAGE  
	WRITE 'PRINT-REC' FROM My-HDR  
	 	AFTER ADVANCING 1 LINE  
	WRITE 'PRINT-REC' FROM Cobol!-HDR   
	 	AFTER ADVANCING 1 LINE  
  
Condensed Header:  
  
01 Hello-HDR.  
	05 FILLER	PIC X(11)	VALUE "Hello World"  
  
01 My-HDR.  
	05 FILLER	PIC X(10)	VALUE "My Name is"   
  
01 Cobol!-HDR.  
	05 FILLER	PIC X(6)	VALUE "Cobol!"  
  
  
write-hdrs.  
	WRITE 'PRINT-REC' FROM Hello-HDR  
		AFTER ADVANCING PAGE  
	WRITE 'PRINT-REC' FROM My-HDR  
		AFTER ADVANCING 1 LINES  
	WRITE 'PRINT-REC' FROM Cobol!-HDR  
		AFTER ADVANCING 1 LINES  

A list of strings can be provided to be used as header titles, if the user defines fewer strings than lines in their header a generic title will be given to the header line instead.

The prefix of the generic header titles can also be changed by the user, for example if the user puts 'my-header' the generated generic headers would be 'my-header1', 'my-header2', and so on.

The user can also set a default name for the write procedure.

There is an option to include indentation in the code generation, this will indent the cobol to the correct area allowing the user to simply place it in their cobol file without needing to edit the code afterwards. This option is checked by default but can be unchecked if the user wishes to add their own indentation.

## Header Title and Write Procedure add/edit/delete view

A Tree view is included to allow the user to quickly add header data field titles and the write procedure title without having to define them in the settings, used if the user wants to just change the titles short term. Titles can be added by right clicking the group title and selecting add, note that only one write procedure title can be in the list at a time. There is also a clear option if the user wants to clear the entries in the group. Individual titles can be edited or deleted by right-clicking on the title item. The whole list can be cleared by clicking the treeview's meatball menu and selecting 'clear all'. 


