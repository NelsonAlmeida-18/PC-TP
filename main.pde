// Configura o ambiente de jogo
void setup() { 
    //size(1280, 720);
    fullScreen(P2D,2);
    smooth();
    startTime = millis(); 
    //Temporário... alterar depois
    players.add(new Player(400, 300)); // Inserir um jogador 
    players.add(new Player(700, 200)); // Inserir um jogador 
    //img = loadImage("/home/carlos/Documents/projeto-pc/background_grass.jpg");
}

// Função principal de renderização do jogo
// É chamada sempre pra atualizar a tela
void draw() { 
    background(0);
    timeConfig();
    
    for (Player player : players) {
        player.drawPlayer(); 
    }
    
    createBonusObject();
    updatePlayer();
    playerToPlayerCollision();
    checkCollision();
    playerCollision();  
    reduceSpeeds(); // Reduzir gradualmente as velocidades do jogador 
    displayPlayerInfo();
    tempTimeExp();
    
}

void keyPressed() {
    
    if (players.isEmpty()) {
        return;
    }
    
    Player player = players.get(0);
    
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
    
    if (players.isEmpty()) {
        return;
    }
    
    Player player = players.get(0);
    
    if (key == 'w') {
        player.moving = false;
    }
}