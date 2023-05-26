import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.util.*;

class TCPClient {
    Socket socket;
    BufferedReader in;
    PrintWriter out;

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

    public void disconnect() throws IOException {
        in.close();
        out.close();
        socket.close();
    }
}

