"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.HeaderTitle = exports.HeaderTitleDataProvider = void 0;
const vscode = require("vscode");
//gives state and behavior to our tree view
class HeaderTitleDataProvider {
    //constructor with dummy data
    constructor() {
        //on change events used to refesh the treeview data
        this._onDidChangeTreeData = new vscode.EventEmitter();
        this.onDidChangeTreeData = this._onDidChangeTreeData.event;
        this.data = [new HeaderTitle('hello', 'headerTitle')];
    }
    //refresh method used to refreah the tree view data
    refresh() {
        this._onDidChangeTreeData.fire(undefined);
    }
    //add tree item allows the user to add another header title to the tree view list, validates that the given string is at least 1 character and 
    //not undefined
    addTreeItem(name) {
        if (name === 'undefined' || name.length === 0) {
            vscode.window.showWarningMessage('Header Title must contain at least one character.');
        }
        else {
            this.data.push(new HeaderTitle(name, 'headerTitle'));
        }
    }
    //edit tree item sets the label of the given HeaderTitle to the given string
    editTreeItem(node, name) {
        node.label = name;
    }
    deleteTreeItem(node) {
        this.data = this.data.filter(item => item != node);
    }
    clearTreeItems() {
        this.data = [];
    }
    //returns a list of all the items in the tree view, used by generate header code to include header titles in tree view as the titles to use for the
    //current instance
    getTreeItemsList() {
        const itemList = [];
        this.data.forEach(item => {
            itemList.push(item.label);
        });
        return itemList;
    }
    //implementation method for TreeDataProvider
    getTreeItem(element) {
        return element;
    }
    //implementation method for TreeDataProvider
    getChildren(element) {
        if (element === undefined) {
            return this.data;
        }
        return element.children;
    }
}
exports.HeaderTitleDataProvider = HeaderTitleDataProvider;
//class for creating HeaderTitle objects, needs the context value so that specific items can be referenced as part of our tree view context
class HeaderTitle extends vscode.TreeItem {
    constructor(label, contextValue, children) {
        super(label, children === undefined ? vscode.TreeItemCollapsibleState.None :
            vscode.TreeItemCollapsibleState.Expanded);
        this.children = children;
        this.contextValue = 'headerTitle';
    }
}
exports.HeaderTitle = HeaderTitle;
//# sourceMappingURL=headerTitles.js.map