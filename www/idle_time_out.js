$(document).on('shiny:connected', function() {
  var timeoutLimit = window.shinyIdleTimeout || 900000; 
  var warningTime = timeoutLimit - 60000; 
  var warningTimer, idleTimer;
  var warningActive = false;
  
  function resetTimer() {
    clearTimeout(warningTimer);
    clearTimeout(idleTimer);
    
    // Only tell R to remove the modal IF it is currently showing
    if (warningActive) {
      Shiny.setInputValue('idle_active', Date.now());
      warningActive = false;
    }
    
    // Start the warning countdown
    warningTimer = setTimeout(function() {
      warningActive = true;
      Shiny.setInputValue('idle_warning', Date.now());
    }, warningTime);
    
    // Start the absolute kill countdown
    idleTimer = setTimeout(function() {
      Shiny.setInputValue('idle_timeout', Date.now());
    }, timeoutLimit);
  }
  
  // Throttle the mousemove event so it doesn't fire constantly
  var throttleTimer;
  function throttledReset() {
    clearTimeout(throttleTimer);
    throttleTimer = setTimeout(resetTimer, 200);
  }
  
  // Reset the timer on any user interaction using the throttled function
  $(document).on('mousemove keydown scroll click', throttledReset);
  
  // Start the timers when the app connects
  resetTimer();
});