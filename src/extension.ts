'use strict';

import * as vscode from 'vscode';

type HeaderRecord = {
	title: string,
	data: string[]
};

export function activate(context: vscode.ExtensionContext) {
	const disposable = vscode.commands.registerCommand('extension.generateHeaderCode', function () {
		// Get the active text editor
		const editor = vscode.window.activeTextEditor;

		//pulls user specified level numbers from user settings
		//need to add condition that fires off an error if the highest !> lowest
		const highestLevel = vscode.workspace.getConfiguration().get('cobolprettyheaders.setHighestLevelNumber');
		const lowestLevel = vscode.workspace.getConfiguration().get('cobolprettyheaders.setLowestLevelNumber');
		const condensed = vscode.workspace.getConfiguration().get('cobolprettyheaders.condensedHeaders');
		const outputLocation = vscode.workspace.getConfiguration().get('cobolprettyheaders.setPrintOutputField');


		if (editor) {
			// start off by getting the document in the current editor and all the text in the document
			const document = editor.document;
			const allText = document.getText();

			// Split the text into individual lines
			const allLines = allText.split("\r\n");
			

			//editor.edit is the main function that edits the current document
			editor.edit(editBuilder => {
				//we define Postion for our calls to insert, this sets the starting line to the number of lines the header is
				const pos = new vscode.Position(allLines.length,0);
				
				//title pulls the first part of the line in the header that is not spaces
				//we may include a setting to allow the user to define these either as a text input box or as a configuration setting
				function getTitle(text: string[]) {
					for (let i = 0; i < text.length; i++) {
						if(text[i].trim().length != 0) {
							return text[i];
						}
					}
				}

				//process line creates a HeaderRecord object for each line, defining the title and data fields
				function processLine(line: string): HeaderRecord  {
					const parts = line.split(/(\s+)/);
					console.log(parts);
					const title: string = getTitle(parts) + '-HDR';
					const data: string[] = [];
					//if the used has set condensed headers to true, the data field will contain the entire line of the header
					if (condensed) {
						data.push('\n\t' + lowestLevel + ' FILLER\tPIC X(' + line.length + ')\tVALUE "' + line + '"');
					}
					else {
						parts.forEach(part => {
							//each data field starts on a new line and is tabbed 1 level in, if the part of the header
							//being defined only contains spaces, a ternary condition sets the value to SPACES
							data.push('\n\t' + lowestLevel + ' FILLER	PIC X(' + part.length +')\tVALUE ' 
							+ (!part.trim().length ? "SPACES" : '"'+ part+ '"'));
							
						});
					}
					//define and return a completed header record object
					const h: HeaderRecord = {title: title, data: data};
					return h;
				}

				//process header returns an array containing HeaderRecords for each line of the header
				function processHeader(lines: string[]): HeaderRecord[] {
					const headerData: HeaderRecord[] = [];
					lines.forEach( line => {
						const processedLine = processLine(line);
						headerData.push(processedLine);
					});
					return headerData;
				}

				//generate data fields does the actual writting of the header datafields to our document
				function generateDataFields(headerData: HeaderRecord[]) {
					headerData.forEach(hData => {
						editBuilder.insert(pos, highestLevel + " " + hData.title + ".");
						for (let i = 0; i < hData.data.length; i++) {
								editBuilder.insert(pos, hData.data[i]);
						}
						//this puts two lines between the data fields and the write procedure
						editBuilder.insert(pos,'\n\n');	
					});
				}

				//generates the write procedure for the header, currently assumes 1 line between each header line, we will add functionality
				//to allow for more than 1 line between header lines, as defined by header text
				function generateWriter(headerData: HeaderRecord[]) {
					editBuilder.insert(pos,'\n');
					editBuilder.insert(pos, '\nwrite-hdrs.');
					for (let i = 0; i < headerData.length; i++) {
						if (i === 0) {
							//first line of the header always has after advancing page, may include setting to change this, not sure
							editBuilder.insert(pos, "\nWRITE '" + outputLocation + "' FROM " + headerData[i].title + '\n\t AFTER ADVANCING PAGE');
						}
						else {
							editBuilder.insert(pos, "\nWRITE '" + outputLocation + "' FROM " + headerData[i].title + '\n\t AFTER ADVANCING 1 LINE');
						}
					}
				}

				//generate code, uses all of the above functions to generate the cobol code to write the header as written in the document
				//by the user
				function generateCode() {
					//creates the header data
					const header = processHeader(allLines);
					//moves down the cursor as to not over write the user submitted text
					for (let i = 0; i < header.length; i++) {
						editBuilder.insert(pos,'\n');
					}
					//write the code
					generateDataFields(header);
					generateWriter(header);
				}

				//activating Generate Headers calls this single function to do so
				generateCode();
			});
		}
	});

	context.subscriptions.push(disposable);
}