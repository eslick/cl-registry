
/****
   Make the internal list of a draggble list sortable. 
   On update, invoke the action using the action code stored in a hidden anchor.
*****/

function initializeDraggableList (widget_id) {
    var handler_id = widget_id + '-handler';
    var list_id = widget_id + '-sortable';
    var code = getDropHandler(handler_id);
    Sortable.create(list_id, { 
	onUpdate: function(list) {
	    var result_string = Sortable.serialize(list);
	    /* This avoids having droppables lying around when we
               replace the content after the ajax returns.  If we
               do an update that doesn't need to re-render the widget
               we have to remove this to keep it alive. */
	    Sortable.destroy(list); 
	    initiateActionWithArgs(code, "", { list: result_string }, "post");
	}
    });
}

/* Extract the action code from the hidden anchor */
function getDropHandler (id) {
    var handler = $(id).getAttribute('handler');
    return handler;
}

/* Call this on page loads to initialize all of the lists */
function initializeDraggableLists () {
    var widgets = $$('.draggable-list');
    widgets.each( function (dlist) { 
	widget_id = dlist.getAttribute('id');
	initializeDraggableList(widget_id); } );
}

/* Setup even to fire on page load (use prototype/lowpro function here) */
Event.onReady( function () { initializeDraggableLists() } );


/******* Old implementation 
  document.observe('dom:loaded', function() { 
  Sortable.create('draglistol', {
    onUpdate: function(list) {
      new Ajax.Request('/update-group', { parameters: Sortable.serialize(list) }); 
    }
  }); 
}); 

********/
