import React from "react";
import ReactDOM from "react-dom";

const App: React.FC = () => {
  return (
    <div>
      <h1>Hello, World!</h1>
      <p>Welcome to your first React app.</p>
    </div>
  );
};

ReactDOM.render(<App />, document.getElementById("root"));
