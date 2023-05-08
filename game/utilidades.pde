int getTimeRemaining() {
    int elapsed = millis() - startTime;
    int remaining = duration - elapsed;
    return max(0, remaining);
}

// Formata o tempo em MIN:SEG
String formatTime(int timeMillis) {
    int seconds = timeMillis / 1000;
    int minutes = seconds / 60;
    seconds %= 60;
    return nf(minutes, 2) + ":" + nf(seconds, 2);
}

// Exibe o tempo no ecrã
void timeConfig() {
    int timeRemaining = getTimeRemaining();
    String timeRemainingString = formatTime(timeRemaining);
    fill(255, 255, 255);
    textSize(20);
    text("Tempo: " + timeRemainingString, 20, 180);
}

// Função temporária (termina o jogo quando acaba o tempo)
// Apenas para teste
void tempTimeExp() {
    int time = getTimeRemaining();
    if(time == 0) {
        exit();
}
}

// Exibir informações do jogador (temporario)
void displayPlayerInfo() {
    
    popMatrix(); 
    pushMatrix();
    resetMatrix();
    fill(255, 255, 255);
    textSize(20);
    text("Velocidade Linear: " + nf(linearSpeedBase, 0, 2), 20, 30);
    text("Velocidade Angular: " + nf(angularSpeedBase, 0, 2), 20, 60);
    text("Pontuação: " + 0.0, 20, 150);
    popMatrix();
}
