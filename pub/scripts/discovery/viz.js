
/*
function drawVisualizationRemote (dom_id, viz_type, action-code, parameters) {
    var url = getActionUrl( code, "", true );
    new Ajax.Request( method: 'get',
		      onSuccess: function () {
			  var json;
			  if(Prototype.Browser.WebKit) {
			      // We should sanitize JSON, but at the moment it crashes Safari
			      json = transport.responseText.evalJSON();
			  } else {
			      json = transport.responseText.evalJSON(true);
			  }
			  
			  drawVisualization (dom_id, viz_type, json, parameters);
		      } );
}
*/

function drawVisualization (dom_id, viz_type, data, parameters) {
    google.load("visualization", "1", {
          packages: ["piechart", "barchart", "columnchart", "linechart", "areachart"], 
          callback: function () { 
	      drawVisualizationAux(dom_id, viz_type, data, parameters);
	  }});
}


function drawVisualizationAux (dom_id, viz_type, data, parameters) {
    var table = importLispDataTable(data);
    var viz = getNewVisualization(viz_type, dom_id);

    params = parameters.evalJSON();
    viz.draw(table, params);
}


function importLispDataTable(json) {
    var lisp_data = json.evalJSON();

    var columns = lisp_data.length;
    var rows = lisp_data[0].length - 1;

    var table = new google.visualization.DataTable();

    for ( var c = 0; c < columns; c++ ) {
	var header = lisp_data[c][0];
	if ( header.length = 2 ) {
	    table.addColumn(header[0], header[1], "");
	} else {
	    table.addColumn(header[0], header[1], header[2]);
	}
    }

    table.addRows(rows);
    for ( var c = 0; c < columns; c++ ) {
	for ( var r = 0; r < rows; r++ ) {
	    table.setValue(r, c, lisp_data[c][r+1]);
	}
    }

    return table;
}


function getNewVisualization (type, id) {
    div = $(id);
    if ( type == "pie-chart-visualization" ) {
	return new google.visualization.PieChart(div);
    } else if ( type == "bar-chart-visualization" ) {
	return new google.visualization.BarChart(div);
    } else if ( type == "column-chart-visualization" ) {
	return new google.visualization.ColumnChart(div);
    } else if ( type == "line-chart-visualization" ) {
	return new google.visualization.LineChart(div);
    } else if ( type == "area-chart-visualization" ) {
	return new google.visualization.AreaChart(div);
    } else if ( type == "wordcloud-visualization" ) {
	return new WordCloud(div);
    } else if ( type == "intensitymap-visualization" ) {
	return new google.visualization.IntensityMap(div);
    } else if ( type == "map-visualization" ) {
	return new google.visualization.Map(div);
    } else {
	alert('No Visualization Found for type: ' + type);
    }
}


// Word Cloud Implementation

/* 

A list of words, where the size and color of each word is determined
by the number of times it appears in the text.
Uses the Google Visalization API.

Data Format
  Any number of rows and columns.
  All string values are concatenated, other column types are ignored.

Configuration options:
  none

Methods
  none

Events
  none

*/

WordCloud = function(container) {
  this.container = container;
}  
  
// Add all word in a given text to a list and map.
// list is a list of unique words.
// map is a set of all found words.
WordCloud.addWords = function(text, list, map) {
  var word = '';
  for (var i = 0; i < text.length; i++) {
    var c = text.charAt(i);
    if (' ,.<>[]{}/`~!@#$%^&*()-_=+\'"\\|:;?\r\r\n'.indexOf(c) >= 0) {
      if (word.length > 0) {
        WordCloud.addWord(word, list, map);
      }
      word = '';
    } else {
      word += c;
    }
  }
  if (word.length > 0) {
    WordCloud.addWord(word, list, map);
  }
};

// Add a single word to a list and map.
// list is a list of unique words.
// map is a set of all found words.
WordCloud.addWord = function(word, list, map) {
  var wl = word.toLowerCase();
  if (map[wl]) {
    map[wl]++;
  } else {
    map[wl] = 1;
    list.push(word);
  }
};
 
WordCloud.MIN_UNIT_SIZE = 1;
WordCloud.MAX_UNIT_SIZE = 7;
WordCloud.RANGE_UNIT_SIZE = WordCloud.MAX_UNIT_SIZE - WordCloud.MIN_UNIT_SIZE;

WordCloud.prototype.draw = function(data, options) {

  var wordMap = {};
  var wordList = [];

  for (var rowInd = 0; rowInd < data.getNumberOfRows(); rowInd++) {
    for (var colInd = 0; colInd < data.getNumberOfColumns(); colInd++) {
      if (data.getColumnType(colInd) == 'string') {
        WordCloud.addWords(data.getValue(rowInd, colInd), wordList, wordMap);
      }
    }
  }
      
  // Compute frequency range
  var minFreq = 999999;
  var maxFreq = 0;
  for (var word in wordMap) {
    var f = wordMap[word];
    minFreq = Math.min(minFreq, f);
    maxFreq = Math.max(maxFreq, f);
  }
  var range = maxFreq - minFreq;
  range = Math.max(range, 4);

  // Idea: Add option to sort by text, freq or no sort

  var html = [];
  html.push('<div class="word-cloud">');
  for (var i = 0; i < wordList.length; i++) {
    var word = wordList[i];
    var text = word;
    var freq = wordMap[word.toLowerCase()];
    var size = WordCloud.MIN_UNIT_SIZE + 
         Math.round((freq - minFreq) / range * WordCloud.RANGE_UNIT_SIZE);
    html.push('<span class="word-cloud-', size, '">', text, '</span> ');
  }
  html.push('</div>');

  this.container.innerHTML = html.join('');
};
