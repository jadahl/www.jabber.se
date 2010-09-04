
function Post()
{
    this.tags = {}; 

    return this;
}

Post.prototype.has_tag = function(tag) {
    var found = false;
    for (var i = 0, m = this.tags.length; i < m; i++)
    {
        if (this.tags[i] == tag)
        {
            found = true;
            break;
        }
    }

    return found;
}

Post.prototype.remove_tag = function(tag) {
    var id = this.tags[tag];
    if (id != undefined)
    {
        delete this.tags[tag];
    }
}

Post.prototype.set_tags = function(tags) {
    this.tags = tags;
}

Post.prototype.add_tag = function(tag, element_id) {
    if (!(tag in this.tags))
    {
        this.tags[tag] = element_id;
    }
}

Post.prototype.validate_new_tag = function(tag) {
    var self = $Site.current_post;
    if (!tag || tag == "")
    {
        return false;
    }

    return !(tag in self.tags);
    //return !(self.has_tag(tag));
}

