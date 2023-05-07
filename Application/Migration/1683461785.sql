CREATE TABLE durations (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    time_duration TEXT NOT NULL
);
