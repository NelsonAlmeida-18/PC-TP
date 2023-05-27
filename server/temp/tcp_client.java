package server.temp;
import java.io.*;
import java.net.*;

public class tcp_client {
    public static void main(String[] args) {
        String serverAddress = "localhost";
        int serverPort = 1234;

        try (Socket socket = new Socket(serverAddress, serverPort);
             BufferedReader input = new BufferedReader(new InputStreamReader(socket.getInputStream()));
             PrintWriter output = new PrintWriter(socket.getOutputStream(), true);
             BufferedReader reader = new BufferedReader(new InputStreamReader(System.in))) {

            System.out.println("Connected to server. Type 'exit' to quit.");

            while (true) {
                System.out.print("> ");
                String message = reader.readLine();
                if (message.equalsIgnoreCase("exit")) {
                    break;
                }

                output.println(message);
                System.out.print("Here");
                String response = input.readLine();
                System.out.println("Server: " + response);

            }

            System.out.println("Disconnected from server.");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
