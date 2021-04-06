"use strict";
//Copyright (C) 2021 Melissa Christie - All Rights Reserved
//You may use, distribute, and modifiy this code under the 
//terms of the MIT License.
Object.defineProperty(exports, "__esModule", { value: true });
exports.HeaderTitle = exports.HeaderTitleDataProvider = void 0;
const vscode = require("vscode");
//gives state and behavior to our tree view
class HeaderTitleDataProvider {
    //constructor with base parent titles for each title group
    constructor() {
        //on change events used to refesh the treeview data
        this._onDidChangeTreeData = new vscode.EventEmitter();
        this.onDidChangeTreeData = this._onDidChangeTreeData.event;
        this.data = [new HeaderTitle('Data Field Titles', 'dataTitle', []), new HeaderTitle('Write Procedure Title', 'writeTitle', [])];
    }
    //refresh method used to refreah the tree view data
    refresh() {
        this._onDidChangeTreeData.fire(undefined);
    }
    //add tree items checks if the title is atleast 1 character long and not undefined, it also checks that the user does not already have a write
    //procedure title since only 1 is allowed, if everything is good it adds a new title to the correct parent using the given index, and gives
    //the new item its context value based on the given index as well
    addTreeItem(name, index) {
        var _a, _b;
        const contextValues = ['dataFieldTitle', 'writeProcedureTitle'];
        if (name === 'undefined' || name.length === 0) {
            vscode.window.showWarningMessage('Header Title must contain at least one character.');
        }
        else if (index === 1 && ((_a = this.data[1].children) === null || _a === void 0 ? void 0 : _a.length) != 0) {
            vscode.window.showWarningMessage('Only one write procedure title can be given');
        }
        else {
            (_b = this.data[index].children) === null || _b === void 0 ? void 0 : _b.push(new HeaderTitle(name, contextValues[index]));
        }
    }
    //edit tree item sets the label of the given HeaderTitle to the given string
    editTreeItem(node, name) {
        node.label = name;
    }
    //deleteTreeItem checks the node's contextValue to determine which parent list to filter and replace
    deleteTreeItem(node) {
        var _a, _b;
        if (node.contextValue === 'dataFieldTitle') {
            this.data[0].children = (_a = this.data[0].children) === null || _a === void 0 ? void 0 : _a.filter(item => item != node);
        }
        else if (node.contextValue === 'writeProcedureTitle') {
            this.data[1].children = (_b = this.data[1].children) === null || _b === void 0 ? void 0 : _b.filter(item => item != node);
        }
    }
    //clear tree items, clears the treeview except for the parent groups
    clear(section) {
        switch (section) {
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
    getTreeItemsList() {
        var _a;
        const itemList = [];
        (_a = this.data[0].children) === null || _a === void 0 ? void 0 : _a.forEach(item => {
            itemList.push(item.label);
        });
        return itemList;
    }
    //getWriteProcedureTitle gets the title to be used in the cobol code generation
    getWriteProcedureTitle() {
        var _a;
        const writeItem = [];
        (_a = this.data[1].children) === null || _a === void 0 ? void 0 : _a.forEach(item => {
            writeItem.push(item.label);
        });
        return writeItem;
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
        this.contextValue = contextValue;
    }
}
exports.HeaderTitle = HeaderTitle;
//# sourceMappingURL=headerTitles.js.map