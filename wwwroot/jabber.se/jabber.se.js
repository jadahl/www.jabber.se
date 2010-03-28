
function set_fragment_path(path)
{
    hi = document.getElementById('page__fragment_path');
    hi.value = path == undefined? window.location.hash : path;
}

function clear_atom_feed_icon(id)
{
    $('#' + id).remove();
}

function set_atom_feed_icon(id, element)
{

    var link = $('#' + id);
    if (link.length == 0)
    {
        var menu_bar_left = $('#menu_bar_left');
        if (menu_bar_left.length == 0)
        {
            // template error
            return;
        }
        else
        {
            menu_bar_left.append(element);
        }
    }
}

