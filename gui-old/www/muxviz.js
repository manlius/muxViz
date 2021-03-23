function rollBox(btnID, divID){

    if($(btnID).attr("class")=="fa fa-caret-square-o-up"){
        $(btnID).attr("class", "fa fa-caret-square-o-down");
    }else{
        $(btnID).attr("class", "fa fa-caret-square-o-up");
    }
    $(divID).slideToggle();
}
