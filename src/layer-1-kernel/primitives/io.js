export const ioPrimitives = {
    'display': (val) => {
        // In REPL, we might want to buffer this
        console.log("DISPLAY:", val);
        return val;
    },
    'newline': () => {
        console.log("");
        return true; // Return a truthy value
    }
};
