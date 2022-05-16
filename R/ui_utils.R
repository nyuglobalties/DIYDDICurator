change_hook <- "function(el,x) {
var hot = this.hot;  
var cellChanges = [];

var changefn = function(changes,source) { 
if (source === 'edit' || source === 'undo' || source === 'autofill' || source === 'paste') {
row = changes[0][0];
col = changes[0][1];
oldval = changes[0][2];
newval = changes[0][3];

if (oldval !== newval) {
  var cell = hot.getCell(row, col);
  cell.style.background = 'yellow';
  cellChanges.push({'rowid':row, 'colid':col});
}
}
}

var renderfn = function(isForced) {

for(i = 0; i < cellChanges.length; i++)
{

var rowIndex = cellChanges[i]['rowid'];
var columnIndex = cellChanges[i]['colid'];

var cell = hot.getCell(rowIndex, columnIndex);
cell.style.background = 'yellow';

}


}

var loadfn = function(initialLoad) {

for(i = 0; i < cellChanges.length; i++)
    {
      var rowIndex = cellChanges[i]['rowid'];
      var columnIndex = cellChanges[i]['colid'];

      var cell = hot.getCell(rowIndex, columnIndex);

      cell.style.background = 'white';

    }
cellChanges = []

}


hot.addHook('afterChange', changefn);
hot.addHook('afterRender', renderfn);
hot.addHook('afterLoadData', loadfn);


}"