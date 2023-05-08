// Configura o ambiente de jogo
void setup() { 
    //size(1280, 720);
    fullScreen(P2D,2);
    smooth();
    startTime = millis();
    img = loadImage("/home/carlos/Documents/projeto-pc/background_grass.jpg");
}

// Função principal de renderização do jogo
// É chamada sempre pra atualizar a tela
void draw() { 
    background(0);
    timeConfig();
    createBonusObject();
    updatePlayer();
    checkCollision();
    playerCollision();
    player.drawPlayer(); // Desenhar jogador  
    reduceSpeeds(); // Reduzir gradualmente as velocidades do jogador 
    displayPlayerInfo();
    tempTimeExp();
    
}

void keyPressed() {
    
    if (key == 'a') {
        player.angle -= angularSpeedBase; // Roda o jogador para a esquerda 
    }
    if (key == 'd') {
        player.angle += angularSpeedBase; // ... ... ... ... direita
    }
    if (key == 'w') {
        player.moving = true;
        player.speed = linearSpeedBase;
    }
}

void keyReleased() {
    if(key == 'w') {
        player.moving = false;
    }
}