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
        ellipse(0, 0, 50, 50); //diametro do jogador 50 unidades
        stroke(0, 255, 0);
        line(0, 0, 25, 0);
        popMatrix();
    }
    
    // Verifica a colisão com outro jogador
    boolean collidesWith(Player other) {
        
        // Calcular a distância entre jogadores    
        float dif_x = this.x - other.x;
        float dif_y = this.y - other.y;
        
        // Distância euclidiana entre os jogadores   
        // seguir a fórmula     
        float distance = sqrt(dif_x * dif_x + dif_y * dif_y);
        
        // Se for maior, não há colisão (false)
        if (distance >= COLLISION_RADIUS) {
            return false;
        }
        
        
        else if (distance < COLLISION_RADIUS) {
            
            //ângulo relativo = ângulo entre a direção de incidência (linha entre os dois jogadores) e a direção (onde o jogador se está a mover)  
            float relativeAngle = atan2(dif_y, dif_x) - this.angle;
            
            // Ajusta o ângulo relativo para ficar entre -PI e PI (atan2 retorna valores entre ..|..)
            while(relativeAngle < - PI) relativeAngle += TWO_PI;
            while(relativeAngle > PI) relativeAngle -= TWO_PI;
            
            // Verifica se o ângulo relativo está entre -pi/2 e pi/2 
            // (vê se está a colidir por trás)
            return abs(relativeAngle) <= PI / 2;
        }
        
        return false;
    }
}

void updatePlayer() {
    
    for (Player player : players) {
        
        // Atualiza a posição do jogador se tiver moving == True
        player.update();
        
        // Atualiza a velocidade do jogador
        player.speed = linearSpeedBase;
        
    }
    
    pushMatrix(); // Salvar a matriz de transformação atual
}

// Verifica a colisão do jogador com a tela
void playerCollision() {
    
    for (Player player : players) {
        //  borda esq         borda dir                borda cima       borda baixo
        if (player.x < 25 || player.x > width - 25 || player.y < 25 || player.y > height - 25) {
            exit();
        }
    }
    
}

//verifica colisão com o outro jogador
//alterar o codigo para quando colidir somar pontos
void playerToPlayerCollision() {
    
    //código teste
    Player player1 = players.get(0);
    
    Player player2 = players.get(1);
    
    if (player1.collidesWith(player2)) {
        exit();
        
    }
    if (player2.collidesWith(player1)) {
        exit();
        
    }
}