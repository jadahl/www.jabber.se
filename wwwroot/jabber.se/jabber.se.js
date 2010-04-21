
function Site()
{
    return this;
}

var $Site = new Site();

Site.prototype.$state_panel_set = function(key, state_panel) {
    // validate
    if (Nitrogen.$validate_and_serialize('.wfid_login_login') == null)
    {
	return;
    }

    // retrieve current and next panel state
    var active = $(state_panel + " > " + ".state_panel_active");
    var next = $(state_panel + " > " + key);

    // update active state
    active.removeClass("state_panel_active");
    next.addClass("state_panel_active");

    // animate transition
    active.slideToggle('fast', function() {
		next.slideToggle('fast');
	    });
}

Site.prototype.$state_panel_show = function(key, state_panel) {
    // retrieve the to be panel state
    var panel = $(state_panel);
    var active = $(state_panel + " > " + key);

    // update active state
    active.addClass("state_panel_active");

    // animate transition
    panel.fadeIn('fast', function() { active.slideDown('fast'); });
}

Site.prototype.$state_panel_hide = function(state_panel) {
    // retrieve current and next panel state
    var panel = $(state_panel);
    var active = $(state_panel + " > " + ".state_panel_active");

    // update active state
    active.removeClass("state_panel_active");

    // animate transition
    active.slideToggle('fast', function() {
	    	panel.fadeOut('fast');
	    });
}

// TODO put the following functions inside class Site.

function clear_form_fields(ids)
{
    for (var id in ids)
    {
	$("#" + id).val("");
    }
}

function get_fragment_path()
{
    var hash = window.location.hash;
    return hash.replace(/#/, "");
}

function set_fragment_path(path)
{
    hi = $('.wfid_fragment_path');
    hi.value = path == undefined? window.location.hash : path;
}

function clear_atom_feed_icon(path)
{
    $(path).remove();
}

function set_atom_feed_icon(path, element)
{
    var link = $(path);
    if (link.length == 0)
    {
        var menu_bar_right = $('#menu_bar_right');
        if (menu_bar_right.length == 0)
        {
            // template error
            return;
        }
        else
        {
            menu_bar_right.append(element);
        }
    }
    else
    {
        link.replaceWith(element);
    }
}

