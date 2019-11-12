$(function() {

$('document').ready(function(){
  console.log('Ready!');
});

// Extend shinydashboard::box
$('.box .btn-box-tool').on('click', function(e){
  console.log(this.getAttribute('data-collapsed'));
});

});
