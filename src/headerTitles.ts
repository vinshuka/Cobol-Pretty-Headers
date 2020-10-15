
import * as vscode from 'vscode';



//gives state and behavior to our tree view
export class HeaderTitleDataProvider implements vscode.TreeDataProvider<HeaderTitle> {

	//on change events used to refesh the treeview data
	private _onDidChangeTreeData: vscode.EventEmitter<HeaderTitle | undefined> = new vscode.EventEmitter<HeaderTitle | undefined>();
	readonly onDidChangeTreeData: vscode.Event<HeaderTitle | undefined> = this._onDidChangeTreeData.event;

	//refresh method used to refreah the tree view data
	refresh(): void {
		this._onDidChangeTreeData.fire(undefined);
	}

	//data variable
	data: HeaderTitle[]
	
	//constructor with base parent titles for each title group
	constructor() {
		this.data = [new HeaderTitle('Data Field Titles', 'dataTitle', []), new HeaderTitle('Write Procedure Title', 'writeTitle', [])];
	}

	//add tree items checks if the title is atleast 1 character long and not undefined, it also checks that the user does not already have a write
	//procedure title since only 1 is allowed, if everything is good it adds a new title to the correct parent using the given index, and gives
	//the new item its context value based on the given index as well
	addTreeItem(name: string, index: number) {
		const contextValues: string[] = ['dataFieldTitle', 'writeProcedureTitle'];
		if (name === 'undefined' || name.length === 0) {
			vscode.window.showWarningMessage('Header Title must contain at least one character.');
		} 
		else if (index === 1 && this.data[1].children?.length != 0) {
			vscode.window.showWarningMessage('Only one write procedure title can be given');
		}
		else {
			this.data[index].children?.push(new HeaderTitle(name, contextValues[index]));
		}
	}

	//edit tree item sets the label of the given HeaderTitle to the given string
	editTreeItem(node: HeaderTitle, name: string) {
		node.label = name;	
	}

	//deleteTreeItem checks the node's contextValue to determine which parent list to filter and replace
	deleteTreeItem(node: HeaderTitle) {
		if (node.contextValue === 'dataFieldTitle') {
			this.data[0].children = this.data[0].children?.filter(item => item != node);
		} 
		else if (node.contextValue === 'writeProcedureTitle') {
			this.data[1].children = this.data[1].children?.filter(item => item != node);
		}
	}

	//clear tree items, clears the treeview except for the parent groups
	clear(section: number) {
		switch(section) {
			case 0: {
				this.data = [new HeaderTitle('Data Field Titles', 'dataTitle', []), new HeaderTitle('Write Procedure Title', 'writeTitle', [])];
				break;
			}
			case 1: {
				this.data = [new HeaderTitle('Data Field Titles', 'dataTitle', []), this.data[1]];
				break;
			}
			case 2: {
				this.data = [this.data[0], new HeaderTitle('Write Procedure Title', 'writeTitle', [])];
				break;
			}
		}
	}

	//returns a list of all the items in the tree view, used by generate header code to include header titles in tree view as the titles to use for the
	//current instance
	getTreeItemsList(): string[] {
		const itemList: string[] = [];
		this.data[0].children?.forEach(item => {
			itemList.push(<string>item.label);
		});
		return itemList;
	}

	//getWriteProcedureTitle gets the title to be used in the cobol code generation
	getWriteProcedureTitle(): string[] {
		const writeItem: string[] = [];
		this.data[1].children?.forEach(item => {
			writeItem.push(<string>item.label);
		});
		return writeItem;
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
export class HeaderTitle extends vscode.TreeItem {
	children: HeaderTitle[] | undefined;

	constructor(label: string, contextValue: string, children?: HeaderTitle[] ) {
		super(
			label,
			children === undefined ? vscode.TreeItemCollapsibleState.None :
			vscode.TreeItemCollapsibleState.Expanded
		);
		this.children = children;
		this.contextValue = contextValue;
	}
}