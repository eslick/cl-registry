
// =========================================
//  Dynamic expansion of summary views
// =========================================    

Event.onReady( function () {
    Event.addBehavior({
	'a.exp-qsum-link:mouseover' : function (e) {
	    expandQsumDetail(Event.findElement(e, '.exp-qsum-head'));
	},
	
	'a.exp-qsum-link:mouseout' : function (e) {
	    contractQsumDetail(Event.findElement(e, '.exp-qsum-head'));
	}
    });

    Event.addBehavior.reassignAfterAjax = true;

});


function expandQsumDetail (element) {
    var detail = element.nextSiblings()[0];
    if ( !detail.visible() ) {
	detail.show();
	element.addClassName('exp-qsum-highlight');
    }
}

function contractQsumDetail (element) {
    var detail = element.nextSiblings()[0];
    if ( detail.visible() ) {
	detail.hide();
	element.removeClassName('exp-qsum-highlight');
    }
}


// =========================================
//  Build a population
// =========================================    

function explorerResultViewInitialize() 
{
    $$('.exp-qsum-head').each(function (s) {
	var sid = s.getAttribute('id');
	new Draggable(sid, { revert: true });
    });
}

function explorerQuestionViewInitialize() 
{
    var header = $('explorer-expanded-header');
    var hid = header.getAttribute('id');
    new Draggable(hid, { revert: true });
    var interior = $('explorer-expanded-interior');
    var iid = interior.getAttribute('id');
    new Draggable(iid, { revert: true });
}

Event.onReady ( function () {
    setTimeout(explorerResultViewInitialize,1);
//    explorerQuestionViewInitialize
});


// ========================================
//  Group Population Creator
// ========================================

function popCreatorInitialize()
{
    var code = $('pop-create-handler').getAttribute('handler');
    
    Droppables.add('pop-create-list', {
	hoverclass: 'pop-create-drop-allowed',
	onDrop: function(thing) {
	    initiateActionWithArgs(code,"",{ item: thing.id }, "post");
	},
	onUpdate: function(list) {
	    var s = list.down('li') ? 'remove' : 'add';
	    list[s + 'ClassName']('empty');
	}
    });
}

Event.onReady ( function () {
    popCreatorInitialize();
});
