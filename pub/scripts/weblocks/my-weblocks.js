var resourceIncludes = {};

function includeResource(url, actions) {
/*    console.log(url); */
    if (resourceIncludes[url])
    {
	return this;
    }

    resourceIncludes[url] = true;

    if(url.indexOf('.css') != -1)
    {
/*      console.log('Loading css for ' + url); */
	YAHOO.util.Get.css(url,actions);
    }
    else
    {
/*	console.log('Loading script for ' + url); */
	YAHOO.util.Get.script(url,actions);
    }

    return true;
}

function includeResources(urls, actions) {
    $A(urls).each( function(url) {
	includeResource(url,actions); 
    } );
}

function flashPane()
{
    $('preferences').highlight();
}