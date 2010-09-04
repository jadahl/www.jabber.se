
function Site()
{
    this.active_dialog = null;

    this.live_post = null;

    this.current_post = null;

    return this;
}

var $Site = new Site();

/*
 * Content management
 */

Site.prototype.$menu_set_current = function(id) {
    // set active menu element
    if (id)
    {
        $("#menu > ul > li > .current").removeClass("current");
        $(id).addClass("current");
    }
}

Site.prototype.$do_load_content = function(url, id) {
    this.$menu_set_current(id);

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
    var active = $(state_panel + " > .state_panel_active");
    var next = $(state_panel + " > " + key);

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

    var prev = $(state_panel + " > .state_panel_active");
    var active = $(state_panel + " > " + key);

    // update active state
    active.addClass("state_panel_active");

    // animate transition
    panel.fadeIn('fast', function() { active.slideDown('fast'); });
}

Site.prototype.$state_panel_hide = function(state_panel) {
    // retrieve current and next panel state
    var panel = $(state_panel);
    var active = $(state_panel + " .state_panel_active");

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
        $('#menu_bar_right').append(element);
    }
    else
    {
        link.replaceWith(element);
    }
}

/*
 * Overlay
 */

Site.prototype.$dialog_hide = function(dialog_id, callback) {
    var dialog = null;
    if (dialog_id)
    {
        dialog = $(dialog_id);
    }
    else
    {
        dialog = this.active_dialog;
        this.active_dialog = null;
    }

    if (dialog && dialog.length == 0)
    {
        $Site.$overlay_hide(callback);
    }
    else if (dialog)
    {
        dialog.fadeOut('fast', function () {
                dialog.remove();
                $Site.$overlay_hide(callback);
                });
    }
    else if (callback)
    {
        callback();
    }
}

Site.prototype.$dialog_show = function(dialog_id, callback) {
    if (this.active_dialog)
    {
        this.active_dialog = null;
        dialog.fadeIn('fast', function() { $Site.show_dialog(dialog_id, callback); });
    }
    else
    {
        var dialog = $(dialog_id);
        this.active_dialog = dialog;
        this.$overlay_show(function() { dialog.fadeIn('fast', function() { dialog.trigger('foo'); if (callback) callback(); }); });
    }
}

Site.prototype.$overlay_hide = function(callback) {
    if (this.active_dialog)
    {
        this.$dialog_hide(null, callback);
    }
    else
    {
        this.overlay.fadeOut('fast', callback);
    }
}

Site.prototype.$set_overlay_size = function() {
    this.overlay.height($(document).height());
    this.overlay.width($(document).width());
}

Site.prototype.$overlay_show = function(callback) {
    this.$set_overlay_size();
    this.overlay.fadeIn('fast', callback);
}

/*
 *
 */

Site.prototype.$new_post = function() {
    this.current_post = new Post();
}

/*
 * Init
 */

Site.prototype.$on_document_loaded = function() {
    // initialize overlay
    this.overlay = $('#overlay');
    this.overlay.click(function() { $Site.$overlay_hide(); });
}

/*
 * History management
 */

onpopstate = function(event) {
    $Site.$history_event(window.location.hash, event.state);
}

/*
 * Init
 */

$(document).ready(function() { $Site.$on_document_loaded(); });

// overlay resizing
$(window).resize(function() { $Site.$set_overlay_size(); });

