class Article_obj:
    def __init__(self):
        self.id = ''
        self.date = ''
        self.article_url = ''
        self.article_title = ''
        self.artefact_url = ''
        self.factchecking_platform = ''
        self.original_platform = ''
        self.post_topic = ''
        self.user_generated_tags = []
        self.keyword_coherence = ''
        self.source_actor = ''
        self.source_role = ''
        self.source_country = ''
        self.content_reference_country = ''
        self.content_creation_purpose = ''
        self.post_sentiment = ''
        self.fVar_artefact = ''
        self.visual_disinformation_category = ''
        self.visual_disinformation_topic = ''
        self.visual_coherence = ''
        self.visual_sentiment = ''
        self.auditive_specifications = ''
        self.visual_cut_tempo = ''
        self.relation_between_post_and_artefact = ''
    
        
    
    def to_dict(self):
        """Method to return the article as a dictionary."""
        return self.__dict__
    