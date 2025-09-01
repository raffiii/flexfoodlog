let connectIndexedDB = (app) => {
    let db;
    let request = indexedDB.open("FlexFoodLog", 3);
    request.onerror = (event) => {
        console.error("Database error: " + event.target.errorCode);
    };
    request.onsuccess = (event) => {
        db = event.target.result;
        console.log("Database initialized");
    };

    // -- Initialize the database schema if needed --
    request.onupgradeneeded = (event) => {
        db = event.target.result;
        let objectStore;
        if (!db.objectStoreNames.contains("events")) {
            objectStore = db.createObjectStore("events", { keyPath: "id", autoIncrement: true });
            objectStore.createIndex("streamId", "streamId", { unique: false });
            objectStore.createIndex("timestamp", "timestamp", { unique: false });
        }
        if (!db.objectStoreNames.contains("streams")) {
            objectStore = db.createObjectStore("streams", { keyPath: "id", autoIncrement: true });
            objectStore.createIndex("name", "name", { unique: true });
        }
        console.log("Database schema initialized");
    }

    // -- Now db should be initialized --

    function runReq(req) {
        return new Promise((resolve, reject) => {
            req.onsuccess = () => resolve(req.result);
            req.onerror = () => reject(req.error || new Error("IDB request failed"));
        });
    }

    function runTx(tx) {
        return new Promise((resolve, reject) => {
            tx.oncomplete = () => resolve();
            tx.onabort = () => reject(tx.error || new Error("IDB transaction aborted"));
            tx.onerror = () => reject(tx.error || new Error("IDB transaction error"));
        });
    }


    app.ports.saveEvent.subscribe(async (envelope) => {
        if (!db) {
            console.error("Database not initialized");
            return;
        }
        // Start a transaction
        let tx = db.transaction(["events", "streams"], "readwrite");
        // Get object stores
        let eventStore = tx.objectStore("events");
        let streamStore = tx.objectStore("streams");

        // Check if a new streamId is needed
        let requiresStreamId = envelope.streamId.slice(-2) === ":*";
        if (requiresStreamId) {
            // Extract base stream name and get or create streamId
            let streamName = envelope.streamId.slice(0, -2);
            let getRequest = await runReq(streamStore.index("name").get(streamName));
            let streamId;
            if (getRequest) {
                // Stream exists, increment nextId
                streamId = getRequest.nextId;
                await runReq(streamStore.put({ id: getRequest.id, name: streamName, nextId: streamId + 1 }));
            } else {
                // Stream does not exist, create it
                streamId = 1;
                await runReq(streamStore.add({ name: streamName, nextId: 2 }));
            }
            envelope.streamId = envelope.streamId.replace("*", streamId);
        }
        await runReq(eventStore.add(envelope));
        await runTx(tx);
    });

}