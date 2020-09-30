# Cobol Pretty Headers

A VS Code extension to generate code required for report headers in Cobol

Use the command pallet (crtl-shift-p) and call the command "Create Header" this will pull up a new text document to write your header in. Once the header is the way you want, use the command pallet again and call "Generate Header Code" the required code for the header data fields and the code to write the header will appear below your header. Simply copy and paste the header data fields into your cobol file under the Working Storage Section and then paste the write code into the Procedure Division. 

# User Defined Settings

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


