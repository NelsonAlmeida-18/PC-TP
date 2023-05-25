class Timer {
  float time;
  
  Timer(float t) {
    time = t;
  }
  
  float getTime() {
    return time;
  }
  
  void setTime(float t) {
    time = t;
  }
  
  void countUp() { // update the timer by counting up. This need to be called within draw() function.
    time += 1/frameRate;
  }
  
  void countDown() { // update the timer by counting down. This need to be called within draw() function.
    time -= 1/frameRate;
  }
  
  
  String formatTime(int time) {
    int seconds = time / 1000;
    int minutes = seconds / 60;
    seconds %= 60;
    
    return nf(minutes, 2) + ":" + nf(seconds, 2);
  }
  
}
