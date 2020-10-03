'use strict';

import * as vscode from 'vscode';

//defining the HeaderRecord type
type HeaderRecord = {
	title: string,
	data: string[]
};

export function activate(context: vscode.ExtensionContext) {

	//declare tree data provider and register it to our treeview
	const provider = new TreeDataProvider();
	vscode.window.registerTreeDataProvider('headerview', provider);
	//tree view commands
	vscode.commands.registerCommand('extension.addEntry', async function () {
		const input = await vscode.window.showInputBox();
		provider.addTreeItem(<string>input);
		provider.refresh();
	});
	vscode.commands.registerCommand('extension.editEntry', async (node: HeaderTitle) => {
		const input = await vscode.window.showInputBox({value: node.label});
		provider.editTreeItem(node, <string>input);
		provider.refresh();
	});
	vscode.commands.registerCommand('extension.deleteEntry', (node: HeaderTitle) => {
		provider.deleteTreeItem(node);
		provider.refresh();
	});

	vscode.commands.registerCommand('extension.clearEntries', function () {
		provider.clearTreeItems();
		provider.refresh();
	});



	const disposable = vscode.commands.registerCommand('extension.generateHeaderCode', function () {
		// Get the active text editor
		const editor = vscode.window.activeTextEditor;

		//get all user specified settings
		//need to add condition that fires off an error if the highest !> lowest
		const highestLevel = vscode.workspace.getConfiguration().get('cobolprettyheaders.setHighestLevelNumber');
		const lowestLevel = vscode.workspace.getConfiguration().get('cobolprettyheaders.setLowestLevelNumber');
		const condensed = vscode.workspace.getConfiguration().get('cobolprettyheaders.condensedHeaders');
		const outputLocation = vscode.workspace.getConfiguration().get('cobolprettyheaders.setPrintOutputField');
		const userDefinedHeaderNames = <string[]>vscode.workspace.getConfiguration().get('cobolprettyheaders.setHeaderNames');
		const defaultNamePrefix = <string>vscode.workspace.getConfiguration().get('cobolprettyheaders.setDefaultNamePrefix');

		//advance is used to keep track of how many lines the write procedure should put between lines
		const advance: number[] = [];

		const treeViewList = provider.getTreeItemsList();
		console.log("List of items in treeview:" + treeViewList.toString());

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

				//generic titles function generates a set of default titles for each line of the header, the user can change the prefix of the generic
				//header titles
				function getGenericTitles(lines: string[]) {
					const titles = [];
					for(let i = 0; i < lines.length; i++) {
						titles.push(defaultNamePrefix + (i+1));
					}
					return titles;
				}

				//process line creates a HeaderRecord object for each line, defining the title and data fields
				function processLine(line: string, headerTitle: string): HeaderRecord  {
					const parts = line.split(/(\s+)/);
					const title: string = headerTitle + '-HDR';
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
				//here is where we will need to deal with line breaks
				//we could check the length of each line and increase a count, pushing it onto an array only when a line that isn't 0 length is found
				function processHeader(lines: string[]): HeaderRecord[] {
					const headerData: HeaderRecord[] = [];
					const genericTitles = getGenericTitles(allLines);
					//here the user given names and the generic names are combined, this is so if the user does not provide enough titles for their
					//header default ones will be given
					//const headerTitles = userDefinedHeaderNames.concat(genericTitles);
					const headerTitles = treeViewList.concat(userDefinedHeaderNames, genericTitles);
					//index used to match header title to line
					let index = 0;
					//line count used to count line advancement for write procedure, increased if the line is a break point, pushed to an array and 
					//reset to 1 if the line is written
					let lineCount = 1;
					lines.forEach( line => {
						if (line.length === 0) {
							lineCount++;
						}
						else {
							const processedLine = processLine(line, headerTitles[index]);
							headerData.push(processedLine);
							advance.push(lineCount);
							lineCount = 1;
							index++;
						}
						
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

				//generates the write procedure for the header, uses advance array generated by the processHeader function to determine how many line 
				//advances are needed for each line of the header
				function generateWriter(headerData: HeaderRecord[]) {
					editBuilder.insert(pos,'\n');
					editBuilder.insert(pos, '\nwrite-hdrs.');
					for (let i = 0; i < headerData.length; i++) {
						if (i === 0) {
							//first line of the header always has after advancing page, may include setting to change this, not sure
							editBuilder.insert(pos, "\n\tWRITE '" + outputLocation + "' FROM " + headerData[i].title + '\n\t\t AFTER ADVANCING PAGE');
						}
						else {
							editBuilder.insert(pos, "\n\tWRITE '" + outputLocation + "' FROM " + headerData[i].title + '\n\t\t AFTER ADVANCING ' + advance[i] + ' LINES');
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

	//called by the user to bring up a new untitled tab to write their header to before calling generate header code to generate the cobol code
	const anotherDisposable = vscode.commands.registerTextEditorCommand('extension.createHeader', function() {

		async function openWindow() {
			const document = await vscode.workspace.openTextDocument();
			vscode.window.showTextDocument(document);
		}
		openWindow();
	});

	context.subscriptions.push(disposable);
	context.subscriptions.push(anotherDisposable);
}


//gives state and behavior to our tree view
class TreeDataProvider implements vscode.TreeDataProvider<HeaderTitle> {

	//on change events used to refesh the treeview data
	private _onDidChangeTreeData: vscode.EventEmitter<HeaderTitle | undefined> = new vscode.EventEmitter<HeaderTitle | undefined>();
	readonly onDidChangeTreeData: vscode.Event<HeaderTitle | undefined> = this._onDidChangeTreeData.event;

	//refresh method used to refreah the tree view data
	refresh(): void {
		this._onDidChangeTreeData.fire(undefined);
	}

	//data variable
	data: HeaderTitle[]
	
	//constructor with dummy data
	constructor() {
		this.data = [new HeaderTitle('hello', 'headerTitle')];
	}

	//add tree item allows the user to add another header title to the tree view list, validates that the given string is at least 1 character and 
	//not undefined
	addTreeItem(name: string) {
		if (name === 'undefined' || name.length === 0) {
			vscode.window.showWarningMessage('Header Title must contain at least one character.');
		} else {
			this.data.push(new HeaderTitle(name, 'headerTitle'));
		}
	}

	//edit tree item sets the label of the given HeaderTitle to the given string
	editTreeItem(node: HeaderTitle, name: string) {
		node.label = name;	
	}

	deleteTreeItem(node: HeaderTitle) {
		this.data = this.data.filter(item => item != node);
	}

	clearTreeItems() {
		this.data = [];
	}

	//returns a list of all the items in the tree view, used by generate header code to include header titles in tree view as the titles to use for the
	//current instance
	getTreeItemsList(): string[] {
		const itemList: string[] = [];
		this.data.forEach(item => {
			itemList.push(<string>item.label);
		});
		return itemList;
	}

	//implementation method for TreeDataProvider
	getTreeItem(element: HeaderTitle): vscode.TreeItem|Thenable<vscode.TreeItem> {
		return element;
	}

	//implementation method for TreeDataProvider
	getChildren(element?: HeaderTitle|undefined): vscode.ProviderResult<HeaderTitle[]> {
		if (element === undefined) {
			return this.data;
		}
		return element.children;
	}
}

//class for creating HeaderTitle objects, needs the context value so that specific items can be referenced as part of our tree view context
class HeaderTitle extends vscode.TreeItem {
	children: HeaderTitle[] | undefined;

	constructor(label: string, contextValue: string, children?: HeaderTitle[] ) {
		super(
			label,
			children === undefined ? vscode.TreeItemCollapsibleState.None :
			vscode.TreeItemCollapsibleState.Expanded
			
			
		);
		this.children = children;
		this.contextValue = 'headerTitle';
		
	}
}