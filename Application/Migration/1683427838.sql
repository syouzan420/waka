CREATE TABLE schtypes (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    schedule_type TEXT NOT NULL
);
