$(function() {
  // Default to 15 mins (900,000 ms) if R hasn't provided a custom limit
  var timeoutLimit = window.shinyIdleTimeout || 900000; 
  var warningTime = timeoutLimit - 60000; // Trigger warning 60 seconds before limit
  var warningTimer, idleTimer;
  
  function resetTimer() {
    clearTimeout(warningTimer);
    clearTimeout(idleTimer);
    
    Shiny.setInputValue('idle_active', Date.now());
    
    // Start the warning countdown
    warningTimer = setTimeout(function() {
      Shiny.setInputValue('idle_warning', Date.now());
    }, warningTime);
    
    // Start the absolute kill countdown
    idleTimer = setTimeout(function() {
      Shiny.setInputValue('idle_timeout', Date.now());
    }, timeoutLimit);
    
  }
  
  // Reset the timer on any user interaction
  $(document).on('mousemove keydown scroll click', resetTimer);
  
  // Start the timer when the page loads
  resetTimer();
});