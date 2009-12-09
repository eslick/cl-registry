function scratchpadInitialize()
{
    var handler_id = 'scratchpad-handler';
    var code = $(handler_id).getAttribute('handler');

    Droppables.add('scratchpad-list', {
	accept: ['question-summary-widget', 'group-summary-widget'],
	hoverclass: 'dropAllowed',
	onDrop: function(thing) {
//            window.console.log("dropped " + thing.id);
	    initiateActionWithArgs(code,"",{ item: thing.id }, "post");
        },
	onUpdate: function(list) {
		    /*
		     * If there are no items in the list, add a
		     * CSS class of "empty".
		     */		     
		    var s = list.down('li') ? 'remove' : 'add';
		    list[s + 'ClassName']('empty');
		}
    });
}

function scratchpadEnableDrag(id)
{
    new Draggable(id, { revert: true });
}

Event.onReady(function() { scratchpadInitialize(); });

