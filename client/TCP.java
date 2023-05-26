import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.util.*;

class TCPClient {
    private Socket socket;
    private BufferedReader in;
    private PrintWriter out;

    public void connect(String serverAddress, int serverPort) throws IOException {
        this.socket = new Socket(serverAddress, serverPort);
        this.in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
        this.out = new PrintWriter(socket.getOutputStream(), true);
    }

    public void sendRequest(String request) throws IOException {
        out.println(request);
    }

    public String receiveResponse() throws IOException {
        return in.readLine();
    }

    public void close() throws IOException {
        in.close();
        out.close();
        socket.close();
    }
}

public class Main {
    public static void main(String[] args) {
        TCPClient cliente = new TCPClient();

        try {
            // Connect to the Erlang server
            client.connect("server_ip_address", 9999); // Replace "server_ip_address" with the actual IP address of the Erlang server

            // Send a request to the Erlang server
            String request = "create_account,JohnDoe,123456"; // Replace with your desired request
            client.sendRequest(request);

            // Receive and print the response from the Erlang server
            String response = client.receiveResponse();
            System.out.println("Response from Erlang server: " + response);
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            try {
                // Disconnect from the Erlang server
                client.disconnect();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }
}


