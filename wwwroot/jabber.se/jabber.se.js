
function Site()
{
    return this;
}

var $Site = new Site();

/*
 * Content management
 */

Site.prototype.$set_current = function(id) {
    // set active menu element
    if (id)
    {
        $("#menu > ul > li > .current").removeClass("current");
        $(id).addClass("current");
    }
}

Site.prototype.$do_load_content = function(url, id) {
    //$Site.$set_current(id);
    this.$set_current(id);

    // request content
    page.load_content(url);
}

Site.prototype.$reload_content = function() {
    location.reload();
    //$Site.$do_load_content(window.location.hash);
}

Site.prototype.$load_content = function(url, id) {
    // don't reload the same page
    if (url == window.location.hash)
    {
        return;
    }

    this.$do_load_content(url, id);
}

Site.prototype.$history_event = function(url, state) {
    // if state is null we we have not set the history entry,
    // thus it's a new entry meaning no page loading
    if (state == null)
    {
        return;
    }
    
    this.$do_load_content(url);
}

Site.prototype.$history_push = function(title, path) {
    // check if function is implemented by the browser
    if (history.replaceState) {
        history.replaceState({ page: path}, title, path);
    }
    else {
        // not supported by browser
    }
}

Site.prototype.$set_title = function(title) {
    document.title = title;
}

/*
 * State panel
 */

Site.prototype.$state_panel_set = function(state_panel, key, animate, validate_group) {
    // validate
    if (validate_group && Nitrogen.$validate_and_serialize(validate_group) == null)
    {
        return;
    }

    // retrieve current and next panel state
    var active = $(state_panel + " > div > .state_panel_active");
    var next = $(state_panel + " > div > " + key);

    // update active state
    active.removeClass("state_panel_active");
    next.addClass("state_panel_active");

    if (animate)
    {
        // animate transition
        active.slideToggle('fast', function() {
                next.slideToggle('fast');
                });
    }
    else
    {
        active.hide();
        next.show();
    }
}

Site.prototype.$state_panel_show = function(key, state_panel) {
    // retrieve the to be panel state
    var panel = $(state_panel);

    if (panel.is(':visible'))
    {
        return;
    }

    var prev = $(state_panel + " > div > .state_panel_active");
    var active = $(state_panel + " > div > " + key);

    // update active state
    active.addClass("state_panel_active");

    // animate transition
    panel.fadeIn('fast', function() { active.slideDown('fast'); });
}

Site.prototype.$state_panel_hide = function(state_panel) {
    // retrieve current and next panel state
    var panel = $(state_panel);
    var active = $(state_panel + " > div > .state_panel_active");

    // update active state
    active.removeClass("state_panel_active");

    // animate transition
    active.slideToggle('fast', function() {
            panel.fadeOut('fast');
            });
}

/*
 * Forms
 */

Site.prototype.$clear_form_fields = function(ids) {
    for (var id in ids)
    {
        $("#" + id).val("");
    }
}

/*
 * Fragment
 */

Site.prototype.$get_fragment_path = function() {
    var hash = window.location.hash;
    return hash.replace(/#/, "");
}

/*
 * Atom icon
 */

Site.prototype.$clear_atom_feed_icon = function(path) {
    $(path).remove();
}

Site.prototype.$set_atom_feed_icon = function(path, element) {
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

/*
 * History management
 */

onpopstate = function(event) {
    $Site.$history_event(window.location.hash, event.state);
}
