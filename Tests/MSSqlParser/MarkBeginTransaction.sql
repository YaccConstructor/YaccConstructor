BEGIN TRANSACTION CandidateDelete
    WITH MARK 'Deleting a Job Candidate';
GO
COMMIT TRANSACTION CandidateDelete;
GO