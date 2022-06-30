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

# function to remove markup
remove_markup <- function(s) {
  s <- str_replace_all(s, "\\\\href\\{[A-Za-z0-9:.\\/]*\\}", "")
  s <- str_replace_all(s, "\\{|\\}", "")
  s <- str_replace_all(s, ":\\\\", ":")
  s <- str_replace_all(s, "doi: ", "doi:")
  s <- str_replace_all(s, "\\\\&amp;", "&")
  s <- str_replace_all(s, "\\\\&;", "&")
  s <- str_replace_all(s, "\\\\url", "")
  s <- str_replace_all(s, "\\\\par ", "")
  s <- str_replace_all(s, "\\\\%", "%")
  s <- str_replace_all(s, ":/ ", ": ")
  s <- str_replace_all(s, "\\\\textbf", "")
  s <- str_replace_all(s, "\\n", "")
  s
}

recurse_read <- function(x) {
  # base case
  if (is.character(x)) {
    x <- remove_markup(x)
  }
  else if (is.list(x)) {
    x <- lapply(x, recurse_read)
  }
  
  x
}

# function to remove markup
add_markup <- function(s) {
  s <- str_replace_all(s, "&", "\\\\&amp;")
  s <- str_replace_all(s, "%", "\\\\%")
  s <- str_replace_all(s, ": ", ":/ ")
  if(nchar(s) == 0 | is.na(s)) s <- NA_character_
  s
}

recurse_write <- function(x) {
  # base case
  if (is.character(x)) {
    x <- add_markup(x)
  }
  else if (is.list(x)) {
    x <- lapply(x, recurse_write)
  }
  
  x
}