function groupEditCreateSortable(widget_id)
{
    var list_id = widget_id + '-sortable';
    var code = $('reorder-group-action-code').value;

    //window.console.log("creating sortable for " + list_id);
    
    Sortable.create(list_id, {
	tag: 'div', only: ['question-summary-widget'],
	constraint: false,
	delay: 500,	// wait 500 ms before triggering drag
	scroll: window,
        onUpdate: function(list) {
            var result = Sortable.serialize(list);
	    initiateActionWithArgs(code, "", { list: result }, "post");
        }
    });
}

function groupEditCreateDroppable(widget_id)
{
    /* For the moment, the whole widget will accept drops (from
     * the scratchpad).
     */
    Droppables.add(widget_id, {
	accept: 'scratchpad-question',
	hoverclass: 'dropAllowed',
	onDrop: function(thing) {
		    window.console.log("received " + thing);

		    var code = $('accept-drop-action-code').value;
		    initiateActionWithArgs(code,"",{ item: thing.id }, "post");
		}
    });
}

function groupEditManageSelection() {
    /* this refers to the element that actually got the event */
    var selector_id = this.id + "-selector";

    window.console.log("got " + this + "id: " + this.id);
//    var code = $(selector_id).getAttribute('handler');

 //   initiateActionWithArgs(code, "", { list: result }, "post");
}


/*
 * This function is called whenever the group widget or
 * any of its constituent question-summary-widgets is
 * re-rendered.  (See the render-widget-body methods for
 * those widgts.)  It would be nice if there was a better way.
 */
function groupEditInitialize() {
    var widgets = $$('.group-editor-widget');

    widgets.each(function (g) {
        var gid = g.getAttribute('id');
        groupEditCreateSortable(gid);
        groupEditCreateDroppable(gid);

/*
	Event.observe(g, 'click', function(event) {
         window.console.log($A(arguments).inspect());
	 window.console.log("in " + g + " click on " + event.element()); });
*/
    });
}

Event.onReady(function() { groupEditInitialize(); });

/* this is screwing with form handling big time.  leave selection
 * out for now, since we don't use it at all. */
/*
Event.addBehavior({
    'div.question-summary-widget:click' : function(e) {
	var elt = e.element();
	if (elt.tagName == 'A' || elt.tagName == 'INPUT' ||
	    elt.tagName == 'TEXTAREA') {
	    return;
        }

	var id = this.id;
	var code = $("update-selection-action-code").value;
        initiateActionWithArgs(code, "", { id: this.id }, "post");

	var questions = $$('.question-summary-widget');
	questions.each(function(qw) {
	    if (qw.id.toString() == id.toString()) {
		qw.addClassName('selected');
	    } else {
		qw.removeClassName('selected');
	    }
	});
    }
});
*/
