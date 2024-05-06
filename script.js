$( window ).on( "load", function() {
  $(".action-button:not(.either-or)").click(function(){
    $(this).toggleClass("active");
  });
  $(".action-button.either-or").click(function(){
    $(this).siblings(".either-or").removeClass("active");
    $(this).addClass("active");
  });
    
});