
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
export class HeaderTitle extends vscode.TreeItem {
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