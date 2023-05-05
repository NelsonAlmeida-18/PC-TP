class Player {
    float x, y, angle, speed;
    boolean moving;
    
    // Construtor que recebe as coordenadas iniciais do jogador
    Player(float x, float y) {   
        this.x = x;  // Coordenadas x do jogador no mundo
        this.y = y;  // Coordenadas y do jogador no mundo
        this.angle = 0;  // Ângulo do jogador em relação ao eixo x radianos
        this.speed = 0; // Velocidade linear do jogador.
        this.moving = false; // Indica se o jogador está em movimento
    }
    // Update na posição do jogador
    void update() {
        if (moving) {
            x += speed * cos(angle);
            y += speed * sin(angle);
        }
    }
    
    // Desenha o player Circulo azul com a respetiva linha verde que é a direção
    void drawPlayer() {  
        pushMatrix();
        translate(x, y);
        rotate(angle);
        fill(0, 0, 255);
        ellipse(0, 0, 50, 50);
        stroke(0, 255, 0);
        line(0, 0, 25, 0);
        popMatrix();
    }
}

void updatePlayer() {
    // Atualiza a posição do jogador se tiver moving == True
    player.update();
    
    // Atualiza a velocidade do jogador
    player.speed = linearSpeedBase;
    
    pushMatrix(); // Salvar a matriz de transformação atual
    
}

// Verifica a colisão do jogador com a tela
void playerCollision() {
    //  borda esq         borda dir                borda cima       borda baixo
    if (player.x < 25 || player.x > width - 25 || player.y < 25 || player.y > height - 25) {
        exit();
}
}