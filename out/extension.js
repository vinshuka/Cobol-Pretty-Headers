'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.activate = void 0;
const vscode = require("vscode");
const headerTitles_1 = require("./headerTitles");
function activate(context) {
    //declare tree data provider and register it to our treeview
    const provider = new headerTitles_1.HeaderTitleDataProvider();
    vscode.window.registerTreeDataProvider('headerview', provider);
    //tree view commands
    vscode.commands.registerCommand('extension.addDataEntry', async function () {
        const input = await vscode.window.showInputBox();
        provider.addTreeItem(input, 0);
        provider.refresh();
    });
    vscode.commands.registerCommand('extension.addWriteEntry', async function () {
        const input = await vscode.window.showInputBox();
        provider.addTreeItem(input, 1);
        provider.refresh();
    });
    vscode.commands.registerCommand('extension.editEntry', async (node) => {
        const input = await vscode.window.showInputBox({ value: node.label });
        provider.editTreeItem(node, input);
        provider.refresh();
    });
    vscode.commands.registerCommand('extension.deleteEntry', (node) => {
        provider.deleteTreeItem(node);
        provider.refresh();
    });
    vscode.commands.registerCommand('extension.clearEntries', function () {
        provider.clear(0);
        provider.refresh();
    });
    vscode.commands.registerCommand('extension.clearDataEntries', function () {
        provider.clear(1);
        provider.refresh();
    });
    vscode.commands.registerCommand('extension.clearWriteEntries', function () {
        provider.clear(2);
        provider.refresh();
    });
    //header strings for placing into other files
    let writeProcedure = "";
    let dataFields = "";
    //theEditor is where the reference to the file where the user creates the header, stored here to be referenced later
    let theEditor;
    //this flag makes sure that the reference is only grabbed once
    let ifEditor = false;
    //maybe create a function to prevent code duplication, this code may be removed or changed later
    vscode.commands.registerCommand('extension.placeDataFields', function () {
        const myeditor = vscode.window.activeTextEditor;
        if (myeditor) {
            const cursor = myeditor.selection.active;
            myeditor.edit(editbuilder => editbuilder.insert(cursor, dataFields));
        }
    });
    vscode.commands.registerCommand('extension.placeWriteProcedure', function () {
        const myeditor = vscode.window.activeTextEditor;
        if (myeditor) {
            const cursor = myeditor.selection.active;
            myeditor.edit(editbuilder => editbuilder.insert(cursor, writeProcedure));
        }
    });
    //so these define functions will only appear when the file they are activated on is a cobol file
    //first they will pull the active text editor and storage variable is either the same filename or if the storage variable is empty
    //this is to make sure that no matter which order the functions are calling in that they are only called on the same file, since
    //data and write code should be on the same file.
    //secondly when defining either location it should check the other location and make sure the user hasn't tried to place them in the same location
    //TODO:This code can be reduced to a couple of functions
    let defineEditor = undefined;
    let dfPos;
    let wpPos;
    vscode.commands.registerCommand('extension.defineDataFieldLocation', function () {
        const myeditor = vscode.window.activeTextEditor;
        if (defineEditor === undefined) {
            defineEditor = myeditor;
        }
        else if ((myeditor === null || myeditor === void 0 ? void 0 : myeditor.document.fileName) != defineEditor.document.fileName) {
            vscode.window.showWarningMessage('Defined data field location must be in the same file as define write procedure location!');
            return;
        }
        if (wpPos != null) {
            if (myeditor) {
                const cursor = myeditor.selection.active;
                if (wpPos.line === cursor.line) {
                    vscode.window.showWarningMessage('You cannot define the same location as the write procedure location!');
                    return;
                }
                else {
                    dfPos = cursor;
                    vscode.window.showInformationMessage('Data field location set.');
                }
            }
        }
        else {
            if (myeditor) {
                const cursor = myeditor.selection.active;
                dfPos = cursor;
                vscode.window.showInformationMessage('Data field location set.');
            }
        }
    });
    vscode.commands.registerCommand('extension.defineWriteProcedureLocation', function () {
        const myeditor = vscode.window.activeTextEditor;
        if (defineEditor === undefined) {
            defineEditor = myeditor;
        }
        else if ((myeditor === null || myeditor === void 0 ? void 0 : myeditor.document.fileName) != defineEditor.document.fileName) {
            vscode.window.showWarningMessage('Defined write procedure location must be in the same file as data fields location!');
            return;
        }
        if (dfPos != null) {
            if (myeditor) {
                const cursor = myeditor.selection.active;
                if (dfPos.line === cursor.line) {
                    vscode.window.showWarningMessage('You cannot define the same location as the data fields location!');
                    return;
                }
                else {
                    wpPos = cursor;
                    vscode.window.showInformationMessage('Write procedure location set.');
                }
            }
        }
        else {
            if (myeditor) {
                const cursor = myeditor.selection.active;
                wpPos = cursor;
                vscode.window.showInformationMessage('Write procedure location set.');
            }
        }
    });
    //using workspace edit we pass the file uri, insert positions, and text to the workspace apply edit function, this allows the user insert their
    //header code at two defined locations
    //TODO: this code needs more edge case testing 
    vscode.commands.registerCommand('extension.insertCode', function () {
        if (defineEditor) {
            const cobolEdit = new vscode.WorkspaceEdit;
            const editUri = vscode.Uri.file(defineEditor.document.fileName);
            cobolEdit.insert(editUri, wpPos, writeProcedure);
            cobolEdit.insert(editUri, dfPos, dataFields);
            vscode.workspace.applyEdit(cobolEdit);
            vscode.window.showInformationMessage('Header code inserted in ' + defineEditor.document.fileName);
        }
    });
    vscode.commands.executeCommand('setContext', 'ext:codeGenerated', false);
    const disposable = vscode.commands.registerCommand('extension.generateHeaderCode', function () {
        vscode.commands.executeCommand('setContext', 'ext:codeGenerated', true);
        // Get the active text editor
        if (!ifEditor) {
            theEditor = vscode.window.activeTextEditor;
            ifEditor = true;
        }
        //setting the editor to the referenced text editor
        const editor = theEditor;
        // const editor = vscode.window.activeTextEditor;
        // console.log('Filename of active editor:' + editor?.document.fileName);
        const treeViewList = provider.getTreeItemsList();
        const writeProcedureName = provider.getWriteProcedureTitle();
        //get all user specified settings
        //need to add condition that fires off an error if the highest !> lowest
        const highestLevel = vscode.workspace.getConfiguration().get('cobolprettyheaders.setHighestLevelNumber');
        const lowestLevel = vscode.workspace.getConfiguration().get('cobolprettyheaders.setLowestLevelNumber');
        const condensed = vscode.workspace.getConfiguration().get('cobolprettyheaders.condensedHeaders');
        const indentation = vscode.workspace.getConfiguration().get('cobolprettyheaders.includeIndentation');
        const outputLocation = vscode.workspace.getConfiguration().get('cobolprettyheaders.setPrintOutputField');
        const userDefinedHeaderNames = vscode.workspace.getConfiguration().get('cobolprettyheaders.setHeaderNames');
        const defaultNamePrefix = vscode.workspace.getConfiguration().get('cobolprettyheaders.setDefaultNamePrefix');
        const userDefinedWriteProcedureName = vscode.workspace.getConfiguration().get('cobolprettyheaders.setWriteProcedureName');
        //we push the userDefinedWriteProcedureName on to the list of names in order to use them in the order of treeview names first then defined
        //names second
        writeProcedureName.push(userDefinedWriteProcedureName);
        //advance is used to keep track of how many lines the write procedure should put between lines
        const advance = [];
        //clear dataFields and Write Procedure strings
        writeProcedure = "";
        dataFields = "";
        //set up the output channel for our header preview
        const preview = vscode.window.createOutputChannel('Header Preview');
        if (editor) {
            // start off by getting the document in the current editor and all the text in the document
            const document = editor.document;
            const allText = document.getText();
            // Split the text into individual lines
            const allLines = allText.split("\r\n");
            //editor.edit is the main function that edits the current document
            editor.edit(editBuilder => {
                //we define Postion for our calls to insert, this sets the starting line to the number of lines the header is
                const pos = new vscode.Position(allLines.length, 0);
                //generic titles function generates a set of default titles for each line of the header, the user can change the prefix of the generic
                //header titles
                function getGenericTitles(lines) {
                    const titles = [];
                    for (let i = 0; i < lines.length; i++) {
                        titles.push(defaultNamePrefix.toUpperCase() + (i + 1));
                    }
                    return titles;
                }
                //process line creates a HeaderRecord object for each line, defining the title and data fields
                function processLine(line, headerTitle) {
                    const parts = line.split(/(\s+)/);
                    const title = headerTitle.toUpperCase() + '-HDR';
                    const data = [];
                    //if the used has set condensed headers to true, the data field will contain the entire line of the header
                    if (condensed) {
                        //if the user has the indentation setting turned on, the indention is automatically included
                        if (indentation) {
                            data.push('\n           ' + lowestLevel + ' FILLER    PIC X(' + line.length + ')    VALUE "' + line + '".');
                        }
                        else {
                            data.push('\n' + lowestLevel + ' FILLER    PIC X(' + line.length + ')    VALUE "' + line + '".');
                        }
                    }
                    else {
                        parts.forEach(part => {
                            //each data field starts on a new line and is tabbed 1 level in, if the part of the header
                            //being defined only contains spaces, a ternary condition sets the value to SPACES
                            //in the case of the user using tab to start their header, a empty string can appear, this condition discards it
                            if (part.length > 0) {
                                if (indentation) {
                                    data.push('\n           ' + lowestLevel + ' FILLER    PIC X(' + part.length + ')    VALUE '
                                        + (!part.trim().length ? "SPACES." : '"' + part + '".'));
                                }
                                else {
                                    data.push('\n' + lowestLevel + ' FILLER    PIC X(' + part.length + ')    VALUE '
                                        + (!part.trim().length ? "SPACES." : '"' + part + '".'));
                                }
                            }
                        });
                    }
                    //define and return a completed header record object
                    const h = { title: title, data: data };
                    return h;
                }
                //process header returns an array containing HeaderRecords for each line of the header
                //here is where we will need to deal with line breaks
                //we could check the length of each line and increase a count, pushing it onto an array only when a line that isn't 0 length is found
                function processHeader(lines) {
                    const headerData = [];
                    const genericTitles = getGenericTitles(allLines);
                    //here the user given names and the generic names are combined, this is so if the user does not provide enough titles for their
                    //header default ones will be given
                    const headerTitles = treeViewList.concat(userDefinedHeaderNames, genericTitles);
                    //index used to match header title to line
                    let index = 0;
                    //line count used to count line advancement for write procedure, increased if the line is a break point, pushed to an array and 
                    //reset to 1 if the line is written
                    let lineCount = 1;
                    lines.forEach(line => {
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
                function generateDataFields(headerData) {
                    dataFields += "\n";
                    headerData.forEach(hData => {
                        if (indentation) {
                            dataFields += "       " + highestLevel + " " + hData.title + ".";
                        }
                        else {
                            dataFields += highestLevel + " " + hData.title + ".";
                        }
                        for (let i = 0; i < hData.data.length; i++) {
                            dataFields += hData.data[i];
                        }
                        //this puts two lines between the data fields and the write procedure
                        dataFields += '\n\n';
                    });
                }
                //generates the write procedure for the header, uses advance array generated by the processHeader function to determine how many line 
                //advances are needed for each line of the header
                function generateWriter(headerData) {
                    if (indentation) {
                        writeProcedure += '\n       ' + writeProcedureName[0].toUpperCase() + '.';
                    }
                    else {
                        writeProcedure += '\n' + writeProcedureName[0].toUpperCase() + '.';
                    }
                    for (let i = 0; i < headerData.length; i++) {
                        if (i === 0) {
                            //first line of the header always has after advancing page, may include setting to change this, not sure
                            if (indentation) {
                                writeProcedure += '\n           WRITE ' + outputLocation + ' FROM ' + headerData[i].title
                                    + '\n               AFTER ADVANCING PAGE.';
                            }
                            else {
                                writeProcedure += '\nWRITE ' + outputLocation + ' FROM ' + headerData[i].title
                                    + '\nAFTER ADVANCING PAGE.';
                            }
                        }
                        else {
                            if (indentation) {
                                writeProcedure += '\n           WRITE ' + outputLocation + ' FROM ' + headerData[i].title
                                    + '\n               AFTER ADVANCING ' + advance[i] + ' LINES.';
                            }
                            else {
                                writeProcedure += '\nWRITE ' + outputLocation + ' FROM ' + headerData[i].title
                                    + '\nAFTER ADVANCING ' + advance[i] + ' LINES.';
                            }
                        }
                    }
                }
                //generate code, uses all of the above functions to generate the cobol code to write the header as written in the document
                //by the user
                function generateCode() {
                    //creates the header data
                    const header = processHeader(allLines);
                    //write the code
                    generateDataFields(header);
                    generateWriter(header);
                }
                //activating Generate Headers calls this single function to do so
                generateCode();
                //shows the currently generated code in the console output for the user to review
                preview.append("Header Data Fields\n=================================\n");
                preview.append(dataFields);
                preview.append("Header Write Procedure\n=================================\n");
                preview.append(writeProcedure);
                preview.show(true);
            });
        }
    });
    //called by the user to bring up a new untitled tab to write their header to before calling generate header code to generate the cobol code
    const anotherDisposable = vscode.commands.registerTextEditorCommand('extension.createHeader', function () {
        async function openWindow() {
            const document = await vscode.workspace.openTextDocument();
            vscode.window.showTextDocument(document);
        }
        //once create header is called it sets the context to isVisable which allows the user to see the tree view for editing titles
        vscode.commands.executeCommand('setContext', 'ext:isVisable', true);
        openWindow();
        //when the user calls create header for the first time or if they call it again after the first time this will set the new window to the editor
        //we will use
        ifEditor = false;
        theEditor = undefined;
    });
    context.subscriptions.push(disposable);
    context.subscriptions.push(anotherDisposable);
}
exports.activate = activate;
//# sourceMappingURL=extension.js.map