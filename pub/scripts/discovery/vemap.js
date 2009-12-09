
Event.onReady( function () {
    setupVemaps();
});

function setupVemaps () {
    if ($('home-map-dialog')) {
	window.myvemapd = new VEMap('home-map-dialog');
	window.myvemapd.LoadMap(new VELatLong(30, 20, 1, VEAltitudeMode.RelativeToGround), 1, VEMapStyle.Road, false, VEMapMode.Mode2D,false,0);
	setupVemap(window.myvemapd);
    } else if ($('home-map')) {
	window.myvemap = new VEMap('home-map');
	window.myvemap.LoadMap(new VELatLong(30, 20, 0, VEAltitudeMode.RelativeToGround), 1, VEMapStyle.Road, true, VEMapMode.Mode2D,false,0);
	window.myvemap.HideDashboard();
	setupVemap(window.myvemap);
    }
};
    
