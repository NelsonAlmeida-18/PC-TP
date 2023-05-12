//Valores máximo do bonus que um player pode obter
float maxSpeed = 10; 
float maxAngle = (PI / 10) * 5;

//Valores iniciais do player
float linearSpeedBase = 2; 
float angularSpeedBase = PI / 10;

//Background Img
PImage img;

// Lista que armazena todos os jogadores 
ArrayList<Player> players = new ArrayList<Player>();

//Lista que armazena todos os objetos bonus do mundo
ArrayList<BonusObject> bonusObjects = new ArrayList<BonusObject>(); 

//Duração de uma partida
int duration = 120 * 1000; 

//Tempo em q a partida começou
int startTime; 

//Distância onde ocorre a colisão entre jogadores
int COLLISION_RADIUS = 50;
