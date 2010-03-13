
function set_fragment_path(path) {
    hi = document.getElementById('page__fragment_path');
    hi.value = path == undefined? window.location.hash : path;
}
