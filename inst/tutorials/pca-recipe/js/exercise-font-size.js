$(function() {
   var editor;
   $('.ace_editor').each(function( index ) {
     editor = ace.edit(this);
     editor.setFontSize("14px");
   });
})
