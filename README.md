# Cobol Pretty Headers

A VS Code extension to generate code required for report headers in Cobol

How To Use:

1. Cobol Pretty Headers generates two pieces of cobol code for headers, the data fields and the write procedure. First indicate where you would like the code to go by placing your cursor on the chosen line and using the vscode command pallet (Ctrl-Shift-p) use command "Set Data Field Location". Repeat this process to set the write procedure location using the command "Set Write Procedure Location" (Note there is a manual placement command if you do not want to predefine locations for the code that will be explained in a later step, skip this step if you want to use the manual option).

2. Use the command pallet and use command "Create Header", a new untitled text tab will open. Here you can type your header exactly how you would like it to appear. Using this command will also cause a new tree view called "Header Titles" to appear in the explorer view. Header Titles can be used to define custom names for the data fields and write procedure of the header. Simply right click on either "Data Field Titles" or "Write Procedure Title" and select Add. You can also edit added titles by right clicking the title and selecting edit. You can delete them in the same manner as well. "Data Field Titles" or "Write Procedure Title" also each have a "clear" option in case you would like to remove the custom titles in that section. There is also a "clear all" option under the meatball menu for the Header Titles tree view.

3. Once you have typed out your header use the command pallet to call the command "Generate Header Code". This will create the cobol code and display a preview of that code in the vscode output window. If you want to change the header simply make the changes and then call the "Generate Header Code" again.

4. If you chose to predefine the locations for the header code all you need to do to all it to your cobol file is use the command pallet to call the command "Insert Code". The generated code will automatically be pasted in the locations you defined. If you chose not to predefine the locations you can insert the code manually, simply place your cursor where you would like the data fields code to go and use the command pallet to call command "Place Header Data Fields". You can do the same with the write procedure code by calling the command "Place Header Write Procedure"

## Header Titles View

A Tree view is included to allow the user to quickly add header data field titles and the write procedure title without having to define them in the settings, used if the user wants to just change the titles short term. Titles can be added by right clicking the group title and selecting add, note that only one write procedure title can be in the list at a time. There is also a clear option if the user wants to clear the entries in the group. Individual titles can be edited or deleted by right-clicking on the title item. The whole list can be cleared by clicking the treeview's meatball menu and selecting 'clear all'. 

## User Defined Settings

Highest and Lowest Level numbers can be set by the user, with the restriction that lowest cannot be a higher level than highest. Default values are `01` for highest and `05` for lowest.

The print output for the write procedure can also be set by the user, it should be the same value that you are writing your report data to. Default value is 'output'.

The user can also select whether or not they would like condensed headers or not.

### Header:
```
Hello World  
My Name is  
Cobol!
```

### Regular header:
```cobol
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
```
  
### Condensed Header:

```cobol
01 Hello-HDR.  
	05 FILLER	PIC X(11)	VALUE "Hello World"  
  
01 My-HDR.  
	05 FILLER	PIC X(10)	VALUE "My Name is"   
  
01 Cobol!-HDR.  
	05 FILLER	PIC X(6)	VALUE "Cobol!"  
  
  
write-hdrs.  
	WRITE 'PRINT-REC' FROM Hello-HDR  
		AFTER ADVANCING PAGE  #
	WRITE 'PRINT-REC' FROM My-HDR  
		AFTER ADVANCING 1 LINES  
	WRITE 'PRINT-REC' FROM Cobol!-HDR  
		AFTER ADVANCING 1 LINES  
```

A list of strings can be provided to be used as header titles, if the user defines fewer strings than lines in their header a generic title will be given to the header line instead.

The prefix of the generic header titles can also be changed by the user, for example if the user puts 'my-header' the generated generic headers would be 'my-header1', 'my-header2', and so on.

The user can also set a default name for the write procedure.

There is an option to include indentation in the code generation, this will indent the cobol to the correct area allowing the user to simply place it in their cobol file without needing to edit the code afterwards. This option is checked by default but can be unchecked if the user wishes to add their own indentation.

